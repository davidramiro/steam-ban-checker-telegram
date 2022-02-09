package com.davidjusto.steambancheck.service;

import com.davidjusto.steambancheck.model.SteamAccount;
import com.davidjusto.steambancheck.model.User;
import com.davidjusto.steambancheck.repository.UserRepo;
import com.pengrad.telegrambot.model.Chat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author davidramiro
 */
@Service
public class UserService {

    private static final Logger LOGGER = LoggerFactory.getLogger(UserService.class);
    private final SteamService steamService;

    private final UserRepo userRepo;

    @Autowired
    public UserService(UserRepo userRepo, SteamService steamService) {
        this.userRepo = userRepo;
        this.steamService = steamService;
    }

    public User saveUser(User user) {
        User dbUser = this.userRepo.findUserByTelegramIdEquals(user.getTelegramId());
        if (dbUser == null) {
            return this.userRepo.save(user);
        }

        dbUser.setWatchedAccounts(user.getWatchedAccounts());
        dbUser.setName(user.getName());
        dbUser.setTelegramId(user.getTelegramId());

        return this.userRepo.save(dbUser);
    }

    public User updateUser(User user) {
        return this.userRepo.save(user);
    }

    public User getUserByTelegramId(Long id) {
        return this.userRepo.findUserByTelegramIdEquals(id);
    }

    public User getUserById(Long id) {
        return this.userRepo.findById(id).orElse(null);
    }

    public boolean checkIfUserExists(Long id) {
        return this.userRepo.existsByTelegramIdEquals(id);
    }

    public void removeAccountFromUser(Long userId, SteamAccount account) {
        User user = this.getUserById(userId);
        if (user != null) {
            user.removeWatchedAccount(account);
            this.userRepo.save(user);
        }
    }

    public User saveUserFromMessage(Chat chat) {
        String name = chat.firstName() == null ? chat.username() : chat.firstName();
        LOGGER.info("Creating new User - Name: {}, Telegram ID: {}", name, chat.id());
        return this.saveUser(new User(chat.id(), name));
    }

    public int addAccountToUser(Long userId, Long accountId) {
        User user = this.getUserById(userId);
        SteamAccount account = this.steamService.getSteamAccountById(accountId);
        user.addWatchedAccount(account);
        user = this.updateUser(user);
        return user.getWatchedAccounts().size();
    }

}
