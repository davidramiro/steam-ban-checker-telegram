package com.davidjusto.steambancheck.model;

import javax.persistence.*;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * @author davidramiro
 */
@Entity(name = "User")
@Table(name = "user")
public class User {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "user_id")
    private Long id;
    private Long telegramId;
    private String name;

    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(
            name = "user_watched_accounts",
            joinColumns = @JoinColumn(name = "user_id"),
            inverseJoinColumns = @JoinColumn(name = "account_id"))
    private Set<SteamAccount> watchedAccounts = new HashSet<>();

    public User() {
    }

    public User(Long telegramId, String name) {
        this.telegramId = telegramId;
        this.name = name;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getTelegramId() {
        return telegramId;
    }

    public void setTelegramId(Long telegramId) {
        this.telegramId = telegramId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Set<SteamAccount> getWatchedAccounts() {
        return watchedAccounts;
    }

    public void setWatchedAccounts(Set<SteamAccount> watchedAccounts) {
        this.watchedAccounts = watchedAccounts;
    }

    public void addWatchedAccount(SteamAccount account) {
        this.watchedAccounts.add(account);
        account.getWatchingUsers().add(this);
    }

    public void removeWatchedAccount(SteamAccount account) {
        this.watchedAccounts.remove(account);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return telegramId.equals(user.telegramId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(telegramId);
    }
}
