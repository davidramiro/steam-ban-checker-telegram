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

import static com.davidjusto.steambancheck.util.SteamApiUtils.getBanInfoJson;

/**
 * @author davidramiro
 */
@Service
@EnableScheduling
public class SteamService {

    private static final Logger LOGGER = LoggerFactory.getLogger(SteamService.class);

    private final String steamApiKey;
    private final SteamAccountRepo steamAccountRepo;

    @Autowired
    public SteamService(@Value("${steam.api.key}") String apiKey, SteamAccountRepo steamAccountRepo) {
        this.steamApiKey = apiKey;
        this.steamAccountRepo = steamAccountRepo;
    }

    boolean checkBan(SteamAccount account) {

        JsonObject singlePlayer = getBanInfoJson(String.valueOf(account.getSteamId()), this.steamApiKey)
                .getAsJsonArray("players").get(0).getAsJsonObject();
        if (singlePlayer.isJsonNull()) {
            LOGGER.error("Account not found or other error.");
        }
        account.setGameBanStatus(singlePlayer.get("NumberOfGameBans").getAsLong() > 0);
        account.setVacBanStatus(singlePlayer.get("NumberOfVACBans").getAsLong() > 0);
        account.setDateAdded(LocalDate.now());

        if (account.getGameBanStatus() && account.getVacBanStatus()) {
            throw new AlreadyBannedException();
        }

        this.save(account);
        return true;
    }

    @Scheduled(fixedRateString = "${polling.interval.ms}", initialDelay = 10000)
    public void scheduledAccountPolling() {
        LOGGER.info("Scheduled polling of tracked steam accounts");
        List<SteamAccount> allAccounts = this.steamAccountRepo.findAll();
        List<List<SteamAccount>> splitByHundreds = Lists.partition(allAccounts, 100);
        List<JsonObject> apiResponses = new ArrayList<>();
        splitByHundreds.forEach(list -> {
            StringBuilder steamIdsParam = new StringBuilder();
            list.forEach(account -> {
                steamIdsParam.append(account.getSteamId());
                steamIdsParam.append(",");
            });
            apiResponses.add(getBanInfoJson(steamIdsParam.toString(), this.steamApiKey));
        });

        apiResponses.forEach(resp -> {
            JsonArray responsePlayers = resp.getAsJsonArray("players");
            responsePlayers.forEach(player -> {
                JsonObject playerObject = player.getAsJsonObject();
                SteamAccount acc = this.steamAccountRepo
                        .findSteamAccountBySteamIdEquals(playerObject.get("SteamId").getAsLong());
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
                    this.steamAccountRepo.save(acc);
                    LOGGER.info("Status changed! Updating db!");
                }
            });
        });
    }

    public SteamAccount getSteamAccountBySteamId(Long id) {
        return this.steamAccountRepo.findSteamAccountBySteamIdEquals(id);
    }

    public SteamAccount getSteamAccountById(Long id) {
        return this.steamAccountRepo.findById(id).orElse(null);
    }

    public SteamAccount save(SteamAccount account) {
        SteamAccount acc = this.steamAccountRepo.findSteamAccountBySteamIdEquals(account.getSteamId());
        if (acc == null) {
            return this.steamAccountRepo.save(account);
        }

        acc.setSteamId(account.getSteamId());
        acc.setVacBanStatus(account.getVacBanStatus());
        acc.setGameBanStatus(account.getGameBanStatus());
        acc.setPublished(account.isPublished());
        acc.setLastBan(account.getLastBan());
        acc.setWatchingUsers(account.getWatchingUsers());

        return this.steamAccountRepo.save(acc);
    }

    public List<SteamAccount> findUnpublishedSteamAccounts() {
        return this.steamAccountRepo.findSteamAccountByIsPublishedFalse();
    }

    public void checkIfOrphanedAndDelete(long steamId) {
        SteamAccount acc = this.steamAccountRepo.findSteamAccountBySteamIdEquals(steamId);
        if (acc.getWatchingUsers().isEmpty()) {
            this.steamAccountRepo.delete(acc);
        }
    }
}
