package com.davidjusto.steambancheck.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

/**
 * @author davidramiro
 */
@SpringBootTest
class SteamApiUtilsTest {

    private static final Long GABEN_STEAMID = 76561197960287930L;

    private final String steamApiKey;

    @Autowired
    public SteamApiUtilsTest(@Value("${steam.api.key}") String steamApiKey) {
        this.steamApiKey = steamApiKey;
    }

    @Test
    void urlShouldReturnCorrectSteamId64() {
        final String fullUrlTrailingSlash = "https://steamcommunity.com/id/gabelogannewell/";
        final String fullUrlNoTrailingSlash = "https://steamcommunity.com/id/gabelogannewell";
        final String fullProfileUrl = "https://steamcommunity.com/profiles/76561197960287930";
        final String noProtocolUrl = "steamcommunity.com/id/gabelogannewell";

        Assertions.assertEquals(GABEN_STEAMID, SteamApiUtils.parseSteamIdFromInput(fullUrlNoTrailingSlash, steamApiKey));
        Assertions.assertEquals(GABEN_STEAMID, SteamApiUtils.parseSteamIdFromInput(fullUrlTrailingSlash, steamApiKey));
        Assertions.assertEquals(GABEN_STEAMID, SteamApiUtils.parseSteamIdFromInput(fullProfileUrl, steamApiKey));
        Assertions.assertEquals(GABEN_STEAMID, SteamApiUtils.parseSteamIdFromInput(noProtocolUrl, steamApiKey));
    }

    @Test
    void invalidUrlShouldReturnErrorCode() {
        final String incompleteUrl = "/id/gabelogannewell/";
        final String missingNumberUrl = "https://steamcommunity.com/profiles/7656119796028793";

        Assertions.assertEquals(-1L, SteamApiUtils.parseSteamIdFromInput(incompleteUrl, steamApiKey));
        Assertions.assertEquals(-1L, SteamApiUtils.parseSteamIdFromInput(missingNumberUrl, steamApiKey));
    }

    @Test
    void steamIdParsingShouldValidate() {
        final String validSteamId = "76561197960287930";
        final String invalidSteamId = "7656119796028793";
        final String invalidNumber = "7656119796O287930";

        Assertions.assertEquals(GABEN_STEAMID, SteamApiUtils.parseSteamIdFromInput(validSteamId, steamApiKey));
        Assertions.assertEquals(-1L, SteamApiUtils.parseSteamIdFromInput(invalidSteamId, steamApiKey));
        Assertions.assertEquals(-1L, SteamApiUtils.parseSteamIdFromInput(invalidNumber, steamApiKey));
    }
}
