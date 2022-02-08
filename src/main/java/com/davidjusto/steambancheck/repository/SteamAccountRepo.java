package com.davidjusto.steambancheck.repository;

import com.davidjusto.steambancheck.model.SteamAccount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @author davidramiro
 */
@Repository
public interface SteamAccountRepo extends JpaRepository<SteamAccount, Long> {
    SteamAccount findSteamAccountBySteamIdEquals(Long id);

    List<SteamAccount> findSteamAccountByIsPublishedFalse();
}
