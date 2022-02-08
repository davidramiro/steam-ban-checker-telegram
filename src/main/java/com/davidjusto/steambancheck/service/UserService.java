package com.davidjusto.steambancheck.service;

import com.davidjusto.steambancheck.model.SteamAccount;
import com.davidjusto.steambancheck.model.User;
import com.davidjusto.steambancheck.repository.UserRepo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author davidramiro
 */
@Service
public class UserService {

    private final UserRepo userRepo;

    @Autowired
    public UserService(UserRepo userRepo, SteamService service) {
        this.userRepo = userRepo;
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
        }
        this.userRepo.save(user);
    }

}
