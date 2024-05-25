const { defineConfig } = require("cypress");
const sqlite3 = require('sqlite3').verbose();

let db;
let week_id;
let allPlayerNames = {};

module.exports = defineConfig({
  pageLoadTimeout: 60000,
  e2e: {
    baseUrl: 'https://wendy-shop.nexters.com',
    setupNodeEvents(on, config) {
      on('task', {
        async getPlayersForCurrentWeek() {
          return new Promise((resolve, reject) => {
            db = new sqlite3.Database('/home/eugene/src/lewwadoo/hero-wars-guild-data/hero-wars-guild-data.db', (err) => {
              if (err) {
                console.log('err', err);
                reject(err);
              } else {
                const current_week_id_query = `SELECT id FROM 'weeks' ORDER BY id DESC LIMIT 1`;
                db.all(current_week_id_query, (err, rows) => {
                  if (err) {
                    reject(err);
                  } else {
                    current_week_id = rows[0].id;
                    const query = `SELECT player_id FROM 'player_week' WHERE week_id = ${current_week_id}`;
                    db.all(query, (err, rows) => {
                      if (err) {
			reject(err);
                      } else {
			// Extract the player_id values into an array
			const playerIds = rows.map((row) => row.player_id);
			resolve(playerIds);
                      }
                      db.close();
		    });
		  }
		});
              }
            });
          });
        },
        async getPlayerNameById(playerId) {
          return new Promise((resolve, reject) => {
            db = new sqlite3.Database('/home/eugene/src/lewwadoo/hero-wars-guild-data/hero-wars-guild-data.db', (err) => {
              if (err) {
                console.log('err', err);
                reject(err);
              } else {
                const query = `SELECT name FROM 'players' WHERE id = ${playerId}`;
                db.get(query, (err, row) => {
                  if (err) {
                    reject(err);
                  } else {
                    resolve(row.name);
                  }
                  db.close();
                });
              }
            });
          });
        },
        async getAllPlayerNames() {
          return new Promise((resolve, reject) => {
            db = new sqlite3.Database('/home/eugene/src/lewwadoo/hero-wars-guild-data/hero-wars-guild-data.db', (err) => {
              if (err) {
                console.log('err', err);
                reject(err);
              } else {
                const query = `SELECT id, name FROM players`;
                db.all(query, (err, rows) => {
                  if (err) {
                    reject(err);
                  } else {
                    rows.forEach((row) => {
                      allPlayerNames[row.id] = row.name.replace(/"/g, '');
                    });
                    resolve(allPlayerNames);
                  }
                  db.close();
                });
              }
            });
          });
        }
      });
    },
  },
});
