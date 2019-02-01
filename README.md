#### Requirements

- `OCaml 4.07+`
- `dune`

#### Dependencies

Run `dune external-lib-deps --missing @install` and install the packages that are missing.

#### Compilation

`make`

#### Running

The following env variables must to be set in order to run the bot:

- `TELEGRAM_BOT_TOKEN`: bot token obtained by @BotFather 
- `TELEGRAM_WEBHOOK_URL`: Telegram webhook url
- `AIRVISUAL_API_KEY`: https://www.airvisual.com api key, to get air quality info
- `OXDICT_APP_ID`, `OXDICT_APP_KEY`: https://oxforddictionaries.com app id and app key, to search the english dictionary
- `DB_CONNECTION_STRING`: connection string for the sqlite3 database in which the bot modules store settings and other data
