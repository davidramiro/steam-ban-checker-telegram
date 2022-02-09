package com.davidjusto.steambancheck.service;

import com.davidjusto.steambancheck.model.SteamAccount;
import com.davidjusto.steambancheck.repository.SteamAccountRepo;
import com.google.common.collect.Lists;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static com.davidjusto.steambancheck.util.SteamApiUtils.getAccountInfoJson;
import static com.davidjusto.steambancheck.util.SteamApiUtils.getBanInfoJson;

/**
 * @author davidramiro
 */
@Service
@EnableScheduling
public class SteamApiService {

    private static final Logger LOGGER = LoggerFactory.getLogger(SteamApiService.class);

    private final String steamApiKey;
    private final SteamService steamService;

    @Autowired
    public SteamApiService(@Value("${steam.api.key}") String apiKey, SteamService steamService) {
        this.steamApiKey = apiKey;
        this.steamService = steamService;
    }

    @Scheduled(fixedRateString = "${polling.interval.ms}", initialDelay = 10000)
    public void scheduledAccountPolling() {
        LOGGER.info("Scheduled polling of tracked steam accounts");
        List<SteamAccount> allAccounts = this.steamService.getAllSteamAccounts();

        List<JsonObject> apiResponses = getJsonFromMultipleSteamAccounts(allAccounts, SteamRequestType.PLAYER_BANS);

        apiResponses.forEach(resp -> {
            JsonArray responsePlayers = resp.getAsJsonArray("players");
            responsePlayers.forEach(player -> {
                JsonObject playerObject = player.getAsJsonObject();
                SteamAccount acc = this.steamService.getSteamAccountBySteamId(playerObject.get("steamid").getAsLong());
                LOGGER.info("Checking account {}", acc.getSteamId());
                boolean newVacBanStatus = playerObject.get("NumberOfVACBans").getAsInt() > 0;
                boolean newGameBanStatus = playerObject.get("NumberOfGameBans").getAsInt() > 0;
                if (Boolean.TRUE.equals(acc.getVacBanStatus()) != newVacBanStatus) {
                    acc.setVacBanStatus(newVacBanStatus);
                    acc.setPublished(false);
                }
                if (Boolean.TRUE.equals(acc.getGameBanStatus()) != newGameBanStatus) {
                    acc.setGameBanStatus(newGameBanStatus);
                    acc.setPublished(false);
                }

                if (Boolean.FALSE.equals(acc.isPublished())) {
                    acc.setLastBan(LocalDate.now());
                    this.steamService.save(acc);
                    LOGGER.info("Ban status changed, updating DB");
                }
            });
        });
    }

    public List<JsonObject> getJsonFromMultipleSteamAccounts(List<SteamAccount> accountList, SteamRequestType type) {
        List<List<SteamAccount>> splitByHundreds = Lists.partition(accountList, 100);
        List<JsonObject> apiResponses = new ArrayList<>();
        splitByHundreds.forEach(list -> {
            StringBuilder steamIdsParam = new StringBuilder();
            list.forEach(account -> {
                steamIdsParam.append(account.getSteamId());
                steamIdsParam.append(",");
            });
            if (type == SteamRequestType.PLAYER_BANS) {
                apiResponses.add(getBanInfoJson(steamIdsParam.toString(), this.steamApiKey));
            } else if (type == SteamRequestType.PLAYER_INFO) {
                apiResponses.add(getAccountInfoJson(steamIdsParam.toString(), this.steamApiKey));
            }
        });

        return apiResponses;
    }

    public List<SteamAccount> getSteamAccountListWithInfos(List<Long> steamIds) {
        List<SteamAccount> selectedAccounts = new ArrayList<>();
        List<SteamAccount> returnAccounts = new ArrayList<>();
        steamIds.forEach(id -> {
            selectedAccounts.add(this.steamService.getSteamAccountBySteamId(id));
        });

        List<JsonObject> apiResponses = getJsonFromMultipleSteamAccounts(selectedAccounts, SteamRequestType.PLAYER_INFO);

        apiResponses.forEach(resp -> {
            JsonArray responsePlayers = resp.getAsJsonArray("players");
            responsePlayers.forEach(player -> {
                JsonObject playerObject = player.getAsJsonObject();
                SteamAccount acc = this.steamService.getSteamAccountBySteamId(playerObject.get("steamid").getAsLong());
                LOGGER.info("Checking account {}", acc.getSteamId());
                acc.setName(playerObject.get("personaname").getAsString());
                acc.setImgUrl(playerObject.get("avatarmedium").getAsString());
                returnAccounts.add(acc);
            });
        });

        return returnAccounts;
    }

    boolean checkBan(SteamAccount account) {

        JsonObject singlePlayer = getBanInfoJson(String.valueOf(account.getSteamId()), this.steamApiKey)
                .getAsJsonArray("players").get(0).getAsJsonObject();
        if (singlePlayer.isJsonNull()) {
            LOGGER.error("Account not found or other error.");
            return false;
        }
        account.setGameBanStatus(singlePlayer.get("NumberOfGameBans").getAsLong() > 0);
        account.setVacBanStatus(singlePlayer.get("NumberOfVACBans").getAsLong() > 0);
        account.setDateAdded(LocalDate.now());

        if (account.getGameBanStatus() && account.getVacBanStatus()) {
            throw new AlreadyBannedException();
        }

        this.steamService.save(account);
        return true;
    }
}
