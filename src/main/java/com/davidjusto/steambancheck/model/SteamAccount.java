package com.davidjusto.steambancheck.model;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * @author davidramiro
 */
@Entity
public class SteamAccount {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "account_id")
    private Long id;
    private Long steamId;
    private Boolean gameBanStatus;
    private Boolean vacBanStatus;
    private LocalDate dateAdded;
    private LocalDate lastBan;
    @Column(nullable = false)
    private Boolean isPublished = true;
    @ManyToMany(mappedBy = "watchedAccounts", fetch = FetchType.EAGER)
    private Set<User> watchingUsers = new HashSet<>();

    public SteamAccount() {
    }

    public SteamAccount(Long steamId) {
        this.steamId = steamId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getSteamId() {
        return steamId;
    }

    public void setSteamId(Long steamId) {
        this.steamId = steamId;
    }

    public Boolean getGameBanStatus() {
        return gameBanStatus;
    }

    public void setGameBanStatus(Boolean gameBanStatus) {
        this.gameBanStatus = gameBanStatus;
    }

    public Boolean getVacBanStatus() {
        return vacBanStatus;
    }

    public void setVacBanStatus(Boolean vacBanStatus) {
        this.vacBanStatus = vacBanStatus;
    }

    public LocalDate getLastBan() {
        return lastBan;
    }

    public void setLastBan(LocalDate lastBan) {
        this.lastBan = lastBan;
    }

    public Boolean isPublished() {
        return isPublished;
    }

    public void setPublished(Boolean published) {
        isPublished = published;
    }

    public Set<User> getWatchingUsers() {
        return watchingUsers;
    }

    public void setWatchingUsers(Set<User> watchingUsers) {
        this.watchingUsers = watchingUsers;
    }

    public LocalDate getDateAdded() {
        return dateAdded;
    }

    public void setDateAdded(LocalDate dateAdded) {
        this.dateAdded = dateAdded;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SteamAccount that = (SteamAccount) o;
        return steamId.equals(that.steamId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(steamId);
    }
}
