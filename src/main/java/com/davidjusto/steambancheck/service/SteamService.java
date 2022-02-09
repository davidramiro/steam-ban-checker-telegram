package com.davidjusto.steambancheck.service;

import com.davidjusto.steambancheck.model.SteamAccount;
import com.davidjusto.steambancheck.repository.SteamAccountRepo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author davidramiro
 */
@Service
public class SteamService {


    private final SteamAccountRepo steamAccountRepo;

    @Autowired
    public SteamService(SteamAccountRepo steamAccountRepo) {
        this.steamAccountRepo = steamAccountRepo;
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

    public List<SteamAccount> getUnpublishedSteamAccounts() {
        return this.steamAccountRepo.findSteamAccountByIsPublishedFalse();
    }

    public List<SteamAccount> getAllSteamAccounts() {
        return this.steamAccountRepo.findAll();
    }

    public void checkIfOrphanedAndDelete(long steamId) {
        SteamAccount acc = this.steamAccountRepo.findSteamAccountBySteamIdEquals(steamId);
        if (acc.getWatchingUsers().isEmpty()) {
            this.steamAccountRepo.delete(acc);
        }
    }
}
