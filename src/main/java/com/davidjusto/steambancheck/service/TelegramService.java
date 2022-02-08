package com.davidjusto.steambancheck.service;

import com.davidjusto.steambancheck.model.SteamAccount;
import com.davidjusto.steambancheck.model.User;
import com.davidjusto.steambancheck.util.SteamApiUtils;
import com.pengrad.telegrambot.TelegramBot;
import com.pengrad.telegrambot.UpdatesListener;
import com.pengrad.telegrambot.model.Chat;
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
    public TelegramService(@Value("${telegram.bot.token}") String token,
                           @Value("${steam.api.key}") String apiKey,
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
                long chatId = update.message().chat().id();
                String command = update.message().text();

                // check if user is known
                User user;
                if (!userService.checkIfUserExists(chatId)) {
                    user = saveUserFromMessage(update.message().chat());
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
        // must be a Steam account then
        long steamId = SteamApiUtils.parseSteamIdFromInput(steamIdInput, this.steamApiKey);
        if (steamId == -1L) {
            bot.execute(new SendMessage(user.getTelegramId(), "Could not parse steam account from your input."));
        } else {
            LOGGER.info("New incoming account request from {} - {}: {}", user.getName(),
                    user.getTelegramId(), steamIdInput);
            SteamAccount acc = checkAndAddSteamAccount(user.getTelegramId(), steamId);
            if (acc != null) {
                int newSize = addAccountToUser(user.getId(), acc.getId());
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
                    "(Example: 76561197960287930) or the full Steam profile URL.\n"));
            return;
        }

        bot.execute(new SendMessage(user.getTelegramId(), "Unknown command. " +
                "Either use /help or send me a Steam account."));

    }

    private SteamAccount checkAndAddSteamAccount(long chatId, long steamId) {
        try {

            boolean apiReturnStatus = this.steamService.checkBan(new SteamAccount(steamId));

            if (!apiReturnStatus) {
                LOGGER.error("Steam API error on ID {}", steamId);
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

    @Scheduled(fixedRateString = "${polling.interval.ms}", initialDelay = 60000)
    public void scheduledAccountPolling() {
        LOGGER.info("Scheduled notification polling");
        List<SteamAccount> unpublishedAccounts = this.steamService.findUnpublishedSteamAccounts();
        unpublishedAccounts.forEach(acc -> {
            LOGGER.info("New ban detected for {}.", acc.getSteamId());
            acc.getWatchingUsers().forEach(user -> {
                LOGGER.info("Notifying Telegram User {} - {}", user.getName(), user.getTelegramId());
                String addedDate = acc.getDateAdded().format(DateTimeFormatter.ofPattern("dd.MM.yyyy"));
                String bannedDate = acc.getLastBan().format(DateTimeFormatter.ofPattern("dd.MM.yyyy"));
                String accInfo = SteamApiUtils.getInfoFromSteamId(acc.getSteamId(), this.steamApiKey);
                bot.execute(new SendMessage(user.getTelegramId(),
                        String.format("New ban detected!%n%s%n%nDate added: %s%nDate banned: %s",
                                accInfo, addedDate, bannedDate)));
                acc.setPublished(true);
                this.steamService.save(acc);
            });
        });
    }

    private User saveUserFromMessage(Chat chat) {
        String name = chat.firstName() == null ? chat.username() : chat.firstName();
        LOGGER.info("Creating new User - Name: {}, Telegram ID: {}", name, chat.id());
        return this.userService.saveUser(new User(chat.id(), name));
    }

    private int addAccountToUser(Long userId, Long accountId) {
        User user = this.userService.getUserById(userId);
        SteamAccount account = this.steamService.getSteamAccountById(accountId);
        user.addWatchedAccount(account);
        user = this.userService.updateUser(user);
        return user.getWatchedAccounts().size();
    }
}
