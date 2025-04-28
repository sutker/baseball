import csv
import sqlite3

conn = sqlite3.connect('database.db')
cursor = conn.cursor()

cursor.execute('DELETE FROM teams')

with open('data/TeamFinalSummary2025.csv', newline='', encoding='utf-8') as csvfile:
    reader = csv.DictReader(csvfile)
    teams = [
        (
            int(row['team_id']),
            row['team_name'],
            int(row['games_played']),
            int(row['wins']),
            int(row['losses']),
            float(row['win_pct']),
            float(row['final_elo']),
            float(row['avg_opp_elo'])
        )
        for row in reader
    ]

cursor.executemany('''
    INSERT INTO teams (
        team_id, team_name, games_played, wins, losses, win_pct, final_elo, avg_opp_elo
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
''', teams)

conn.commit()
conn.close()

print("✅ Full team Elo data imported.")
