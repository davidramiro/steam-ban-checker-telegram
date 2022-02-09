package com.davidjusto.steambancheck.controller;

import com.davidjusto.steambancheck.service.SteamApiService;
import com.davidjusto.steambancheck.service.UserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.ArrayList;
import java.util.List;

/**
 * @author davidramiro
 */
@Controller
public class TrackedAccountsController {

    private static final Logger LOGGER = LoggerFactory.getLogger(TrackedAccountsController.class);

    private final UserService userService;
    private final SteamApiService steamApiService;

    @Autowired
    public TrackedAccountsController(UserService userService, SteamApiService steamApiService) {
        this.userService = userService;
        this.steamApiService = steamApiService;
    }

    @GetMapping("/tracked")
    public String tracked(@RequestParam(name= "telegramId") Long telegramId, Model model) {

        LOGGER.info("Account List request from {}", telegramId);
        List<Long> steamIds = new ArrayList<>();
        this.userService.getUserByTelegramId(telegramId).getWatchedAccounts()
                .forEach(acc -> steamIds.add(acc.getSteamId()));
        model.addAttribute("accounts", this.steamApiService.getSteamAccountListWithInfos(steamIds));
        return "tracked";
    }
}
