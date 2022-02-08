package com.davidjusto.steambancheck.service;

import com.davidjusto.steambancheck.model.SteamAccount;
import com.davidjusto.steambancheck.model.User;
import com.davidjusto.steambancheck.util.SteamApiUtils;
import com.pengrad.telegrambot.TelegramBot;
import com.pengrad.telegrambot.UpdatesListener;
import com.pengrad.telegrambot.model.Update;
import com.pengrad.telegrambot.request.SendMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.format.DateTimeFormatter;
import java.util.List;


/**
 * @author davidramiro
 */
@Service
@EnableScheduling
public class TelegramService {

    private static final Logger LOGGER = LoggerFactory.getLogger(TelegramService.class);

    private final TelegramBot bot;
    private final SteamService steamService;
    private final UserService userService;
    private final String steamApiKey;

    @Autowired
    public TelegramService(@Value("${telegram.bot.token}") String token, @Value("${steam.api.key}") String apiKey,
                           SteamService steamService, UserService userService) {
        this.userService = userService;
        this.steamApiKey = apiKey;
        this.steamService = steamService;
        bot = new TelegramBot(token);
        this.setUpListener();
    }

    public void setUpListener() {
        bot.setUpdatesListener(updateList -> {

            for (Update update : updateList) {
                if (update.message().text().startsWith("/")) {
                    bot.execute(new SendMessage(update.message().chat().id(),
                            this.getResponseForUserCommand(update.message().text())));
                    continue;
                }

                User user;

                if (!userService.checkIfUserExists(update.message().chat().id())) {
                    user = saveUserFromMessage(update);
                } else {
                    user = this.userService.getUserByTelegramId(update.message().chat().id());
                }

                SteamAccount acc = checkBanFromMessage(update);
                if (acc == null) {
                    continue;
                }
                int newSize = addAccountToUser(user.getId(), acc.getId());
                bot.execute(new SendMessage(user.getTelegramId(), String.format("Currently watched accounts: %d", newSize)));
            }

            return UpdatesListener.CONFIRMED_UPDATES_ALL;
        });
    }

    private String getResponseForUserCommand(String text) {
        if (text.toLowerCase().contains("start")) {
            return "Send me a Steam account to keep track of its ban status. You can either send me profile URLs or " +
                    "their SteamID64.";
        }

        return "Unknown command. Either use /start or send me a Steam account.";

    }

    private SteamAccount checkBanFromMessage(Update update) {
        long chatId = update.message().chat().id();
        try {
            long steamId = SteamApiUtils.parseSteamIdFromInput(update.message().text(), this.steamApiKey);
            if (steamId == -1L) {
                bot.execute(new SendMessage(chatId, "Could not parse steam account from your input."));
                return null;
            }

            boolean apiReturnStatus = this.steamService.checkBan(new SteamAccount(steamId));

            if (!apiReturnStatus) {
                LOGGER.error("Steam API error on message {}", update.message().text());
                bot.execute(new SendMessage(chatId, "Steam API error."));
                return null;
            }

            SteamAccount returnedAccount = steamService.getSteamAccountBySteamId(steamId);
            String accountInfo = SteamApiUtils.getInfoFromSteamId(steamId, this.steamApiKey);

            bot.execute(new SendMessage(chatId, String.format("Added account:%n%s", accountInfo)));

            return returnedAccount;

        } catch (AlreadyBannedException abe) {
            bot.execute(new SendMessage(chatId, "Already Game and VAC banned. Nothing to track, not saved."));
        }

        return null;
    }

    @Scheduled(fixedRateString ="${polling.interval.ms}", initialDelay=10000)
    public void scheduledAccountPolling() {
        LOGGER.info("Scheduled notification polling");
        List<SteamAccount> unpublishedAccounts = this.steamService.findUnpublishedSteamAccounts();
        unpublishedAccounts.forEach(acc -> {
            acc.getWatchingUsers().forEach(user -> {
                String addedDate = acc.getDateAdded().format(DateTimeFormatter.ofPattern("dd/MM/yyyy"));
                String bannedDate = acc.getLastBan().format(DateTimeFormatter.ofPattern("dd/MM/yyyy"));
                String accInfo = SteamApiUtils.getInfoFromSteamId(acc.getSteamId(), this.steamApiKey);
                bot.execute(new SendMessage(user.getTelegramId(),
                        String.format("New ban detected!%n%s%n%nDate added: %s%nDate banned: %s",
                                accInfo, addedDate, bannedDate)));
                acc.setPublished(true);
                this.steamService.save(acc);
            });
        });
    }

    private User saveUserFromMessage(Update update) {

        String name = update.message().chat().firstName() == null ?
                update.message().chat().username() : update.message().chat().firstName();
        return this.userService.saveUser(new User(update.message().chat().id(), name));
    }

    private int addAccountToUser(Long userId, Long accountId) {
        User user = this.userService.getUserById(userId);
        SteamAccount account = this.steamService.getSteamAccountById(accountId);
        user.addWatchedAccount(account);
        user = this.userService.updateUser(user);
        return user.getWatchedAccounts().size();
    }
}
