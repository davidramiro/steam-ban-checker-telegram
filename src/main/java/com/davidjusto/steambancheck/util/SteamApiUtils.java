package com.davidjusto.steambancheck.util;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * @author davidramiro
 */
public class SteamApiUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(SteamApiUtils.class);
    private static final RestTemplate REST_TEMPLATE = new RestTemplateBuilder().build();
    private static final String STEAM_API_BASE_URL = "https://api.steampowered.com/ISteamUser/";
    private static final String STEAM_BAN_API_URL = "GetPlayerBans/v1/?key=%s&steamids=%s";
    private static final String STEAM_VANITY_ID_API_URL = "ResolveVanityURL/v0001/?key=%s&vanityurl=%s";
    private static final String STEAM_INFO_API_URL = "GetPlayerSummaries/v0002/?key=%s&steamids=%s";

    private SteamApiUtils() {
    }

    public static JsonObject getBanInfoJson(String steamIds, String steamApiKey) {
        String json = REST_TEMPLATE.getForObject(String.format(STEAM_API_BASE_URL + STEAM_BAN_API_URL,
                steamApiKey, steamIds), String.class);
        return new Gson().fromJson(json, JsonObject.class);
    }

    public static Long parseSteamIdFromInput(String input, String apiKey) {
        Long parsedId = 0L;

        if (input.toLowerCase().contains("steamcommunity")) {
            URI uri = null;
            try {
                uri = new URI(input);
                if (input.toLowerCase().contains("/profiles/")) {
                    String[] segments = uri.getPath().split("/");
                    String idStr = segments[segments.length - 1];
                    parsedId = Long.parseLong(idStr);
                }

                if (input.toLowerCase().contains("/id/")) {
                    String[] segments = uri.getPath().split("/");
                    String idStr = segments[segments.length - 1];
                    parsedId = getSteamIdFromVanityUrl(idStr, apiKey);
                }
            } catch (URISyntaxException use) {
                LOGGER.error("Malformed URL input: {}", input);
                return -1L;
            } catch (NumberFormatException nfe) {
                LOGGER.error("Unexpected non-numeric part in URL: {}", input);
                return -1L;
            }
        } else {
            try {
                parsedId = Long.parseLong(input);
            } catch (NumberFormatException nfe) {
                LOGGER.error("Could not parse 64bit Long from non-URL: {}", input);
                return -1L;
            }
        }

        if (parsedId.toString().matches("^[0-9]{17}$")) {
            return parsedId;
        } else {
            LOGGER.error("Unknown parser error: {}", input);
            return -1L;
        }

    }

    private static Long getSteamIdFromVanityUrl(String vanityUrl, String steamApiKey) {
        String json = REST_TEMPLATE.getForObject(
                String.format(STEAM_API_BASE_URL + STEAM_VANITY_ID_API_URL, steamApiKey, vanityUrl), String.class);
        JsonObject response = new Gson().fromJson(json, JsonObject.class)
                .get("response").getAsJsonObject();
        if (response.get("success").getAsInt() == 1) {
            return response.get("steamid").getAsLong();
        } else {
            return -1L;
        }
    }

    public static String getInfoFromSteamId(Long steamId, String steamApiKey) {
        String json = REST_TEMPLATE.getForObject(String.format(STEAM_API_BASE_URL + STEAM_INFO_API_URL,
                steamApiKey, steamId), String.class);
        JsonArray playerInfos = new Gson().fromJson(json, JsonObject.class)
                .get("response").getAsJsonObject()
                .get("players").getAsJsonArray();
        if (playerInfos.size() != 1) {
            throw new SteamApiException();
        }

        JsonObject playerInfo = playerInfos.get(0).getAsJsonObject();
        StringBuilder sb = new StringBuilder("SteamID64: ");
        sb.append(playerInfo.get("steamid").getAsString());
        sb.append("\nProfile Name: ");
        sb.append(playerInfo.get("personaname").getAsString());
        sb.append("\nProfile URL: ");
        sb.append(playerInfo.get("profileurl").getAsString());

        return sb.toString();
    }

}
