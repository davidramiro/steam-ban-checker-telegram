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
    private static final String DATE_FORMAT = "dd.MM.yyyy";

    private final TelegramBot bot;
    private final SteamService steamService;
    private final UserService userService;
    private final String steamApiKey;
    private final SteamApiService steamApiService;
    private final String baseUrl;
    private final boolean usesWebpage;

    @Autowired
    public TelegramService(@Value("${telegram.bot.token}") String token,
                           @Value("${steam.api.key}") String apiKey,
                           @Value("${app.base.url}") String baseUrl,
                           @Value("${app.enable.webpage}") boolean usesWebpage,
                           SteamService steamService, UserService userService, SteamApiService steamApiService) {
        this.userService = userService;
        this.steamApiKey = apiKey;
        this.steamService = steamService;
        this.steamApiService = steamApiService;
        this.baseUrl = baseUrl;
        this.usesWebpage = usesWebpage;
        bot = new TelegramBot(token);
        this.setUpListener();
    }

    public void setUpListener() {
        bot.setUpdatesListener(updateList -> {

            for (Update update : updateList) {
                if (update.message() == null) {
                    return update.updateId();
                }
                long chatId = update.message().chat().id();
                String command = update.message().text();

                // check if user is known
                User user;
                if (!userService.checkIfUserExists(chatId)) {
                    user = this.userService.saveUserFromMessage(update.message().chat());
                } else {
                    user = this.userService.getUserByTelegramId(update.message().chat().id());
                }

                // check if it's a command
                if (update.message().text().startsWith("/")) {
                    this.handleCommand(command, user);
                } else {
                    this.handleSteamId(command, user);
                }
            }

            return UpdatesListener.CONFIRMED_UPDATES_ALL;
        });
    }

    private void handleSteamId(String steamIdInput, User user) {
        long steamId = SteamApiUtils.parseSteamIdFromInput(steamIdInput, this.steamApiKey);
        if (steamId == -1L) {
            bot.execute(new SendMessage(user.getTelegramId(), "Could not parse steam account from your input."));
        } else {
            LOGGER.info("New incoming account request from {} - {}: {}", user.getName(),
                    user.getTelegramId(), steamIdInput);
            SteamAccount acc = checkAndAddSteamAccount(user.getTelegramId(), steamId);
            if (acc != null) {
                int newSize = this.userService.addAccountToUser(user.getId(), acc.getId());
                LOGGER.info("User {} - {} is now tracking {} accounts.", user.getName(),
                        user.getTelegramId(), newSize);
                bot.execute(new SendMessage(user.getTelegramId(), String.format("Currently watched accounts: %d", newSize)));
            }
        }
    }

    private void handleCommand(String command, User user) {
        if (command.toLowerCase().contains("start")) {
            bot.execute(new SendMessage(user.getTelegramId(), "Send me a Steam account to keep track of its ban " +
                    "status. You can either send me profile URLs or their SteamID64."));
            return;
        }

        if (command.toLowerCase().contains("help")) {
            bot.execute(new SendMessage(user.getTelegramId(), "Add Steam accounts by either submitting the SteamID64" +
                    "(Example: 76561197960287930) or the full Steam profile URL.\n\n" +
                    "List watched Steam accounts by using /showtracked\n\n" +
                    "Remove tracked Steam accounts by using /remove <SteamID64/Profile URL>"));
            return;
        }

        if (command.toLowerCase().contains("showtracked")) {
            if (this.usesWebpage) {
                StringBuilder url = new StringBuilder(baseUrl);
                url.append("/tracked?telegramId=");
                url.append(user.getTelegramId());
                bot.execute(new SendMessage(user.getTelegramId(), "Check your tracked accounts by visiting " +
                        url));
            } else {
                showTrackedAccountsForUser(user);
            }

            return;
        }

        if (command.toLowerCase().contains("remove")) {
            String[] commandParts = command.split("\\s+");
            if (commandParts.length != 2) {
                bot.execute(new SendMessage(user.getTelegramId(), "Invalid input. Please remove Steam accounts " +
                        "by using /remove <SteamID64/Profile URL>"));
            } else {
                Long steamId = SteamApiUtils.parseSteamIdFromInput(commandParts[1], this.steamApiKey);
                if (steamId == -1L) {
                    bot.execute(new SendMessage(user.getTelegramId(), "Could not parse steam account from your input."));

                } else {
                    this.removeAccountTrackingFromUser(user, steamId);
                }
            }

            return;
        }

        bot.execute(new SendMessage(user.getTelegramId(), "Unknown command. " +
                "Either use /help or send me a Steam account."));

    }

    private void removeAccountTrackingFromUser(User user, Long steamId) {
        try {
            this.userService.removeAccountFromUser(user.getId(), this.steamService.getSteamAccountBySteamId(steamId));
            this.steamService.checkIfOrphanedAndDelete(steamId);
        } catch (NullPointerException npe) {
            bot.execute(new SendMessage(user.getTelegramId(), "Error on deletion. " +
                    "Did you really track this user before?"));
            return;
        }

        bot.execute(new SendMessage(user.getTelegramId(), String.format("Account with Steam ID %s " +
                "successfully removed from your tracked accounts.", steamId)));
    }

    private void showTrackedAccountsForUser(User user) {
        user.getWatchedAccounts().forEach(acc -> {
            String accountInfo = SteamApiUtils.getInfoText(acc.getSteamId(), this.steamApiKey);
            String added = acc.getDateAdded().format(DateTimeFormatter.ofPattern(DATE_FORMAT));
            String banned = acc.getLastBan() != null ?
                    acc.getLastBan().format(DateTimeFormatter.ofPattern(DATE_FORMAT)) : "Not banned";
            bot.execute(new SendMessage(user.getTelegramId(), String.format("%s%nDate added: %s%nBanned: %s",
                    accountInfo, added, banned)));
        });
    }

    private SteamAccount checkAndAddSteamAccount(long chatId, long steamId) {
        try {

            boolean apiReturnStatus = this.steamApiService.checkBan(new SteamAccount(steamId));

            if (!apiReturnStatus) {
                LOGGER.error("Steam API error on ID {}", steamId);
                bot.execute(new SendMessage(chatId, "Steam API error."));
                return null;
            }

            SteamAccount returnedAccount = steamService.getSteamAccountBySteamId(steamId);
            String accountInfo = SteamApiUtils.getInfoText(steamId, this.steamApiKey);

            bot.execute(new SendMessage(chatId, String.format("Added account:%n%s", accountInfo)));

            return returnedAccount;

        } catch (AlreadyBannedException abe) {
            bot.execute(new SendMessage(chatId, "Already Game and VAC banned. Nothing to track, not saved."));
        }

        return null;
    }

    @Scheduled(fixedRateString = "${polling.interval.ms}", initialDelay = 60000)
    public void scheduledAccountPolling() {
        LOGGER.info("Scheduled notification polling");
        List<SteamAccount> unpublishedAccounts = this.steamService.getUnpublishedSteamAccounts();
        unpublishedAccounts.forEach(acc -> {
            LOGGER.info("New ban detected for {}.", acc.getSteamId());
            acc.getWatchingUsers().forEach(user -> {
                LOGGER.info("Notifying Telegram User {} - {}", user.getName(), user.getTelegramId());
                String addedDate = acc.getDateAdded().format(DateTimeFormatter.ofPattern(DATE_FORMAT));
                String bannedDate = acc.getLastBan().format(DateTimeFormatter.ofPattern(DATE_FORMAT));
                String accInfo = SteamApiUtils.getInfoText(acc.getSteamId(), this.steamApiKey);
                bot.execute(new SendMessage(user.getTelegramId(),
                        String.format("New ban detected!%n%s%n%nDate added: %s%nDate banned: %s",
                                accInfo, addedDate, bannedDate)));
                acc.setPublished(true);
                this.steamService.save(acc);
            });
        });
    }


}
