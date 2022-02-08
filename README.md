# Steam Ban Checker Telegram Bot

Spring Boot application that serves a Telegram bot tracking Steam accounts and notifying users on VAC or game bans. Tested with Java 8, 11 and 17.

## Requirements
- Telegram Bot token (Create a bot on [Telegram](https://t.me/botfather))
- Steam Web API Key (Get yours on [Steam](https://steamcommunity.com/dev/apikey))
- MariaDB/MySQL database and Java 8+ installed on the machine to run this application on

## Usage

- Clone the repo, enter token, API key and DB settings on `application.properties`, issue `mvn package` and take the jar file from `target` directory
- Run the application with `java -jar steambancheck-<VERSION>.jar`
- Contact the bot on Telegram


## Linux systemd Service

To create a systemd service and run the application on boot, create a service file, for example under
`/etc/systemd/system/steambancheck.service`.

Service file contents:
```
[Unit]
Description=Steam Ban Checker Telegram Bot

[Service]
WorkingDirectory=/your/path
ExecStart=/bin/java -jar steambancheck-VERSION.jar
User=youruser
Type=simple
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Don't run this as root. Make sure your `User` has access rights to the `WorkingDirectory` where the jar file is in.

Reload daemon, start the service, check its status:

```
sudo systemctl daemon-reload
sudo systemctl start steambancheck.service
sudo systemctl status steambancheck
```

If all is well, enable the service to be started on boot:

`sudo systemctl enable steambancheck`

