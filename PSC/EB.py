from slack_sdk import WebClient
import sqlite3
from datetime import datetime, timedelta
import os

SLACK_TOKEN = os.getenv("SLACK_TOKEN", "your-slack-bot-token")
client = WebClient(token=SLACK_TOKEN)

def fetch_messages(C0883QT1Y84, days=30):
    cutoff = (datetime.now() - timedelta(days=days)).timestamp()
    response = client.conversations_history(channel=channel_id, oldest=cutoff)
    messages = response["messages"]
    
    conn = sqlite3.connect("engagement.db")
    c = conn.cursor()
    for msg in messages:
        c.execute("SELECT COUNT(*) FROM messages WHERE timestamp = ?", (msg["ts"],))
        if c.fetchone()[0] == 0:
            c.execute("INSERT INTO messages VALUES (?, ?, ?, ?)",
                      (msg["ts"], msg["user"], C0883QT1Y84, msg.get("text", "")))
            # Update member message count and points
            c.execute("INSERT OR REPLACE INTO members (user_id, region, message_count, points) 
                       VALUES (?, 'Unknown', COALESCE((SELECT message_count FROM members WHERE user_id = ?), 0) + 1, 
                       COALESCE((SELECT points FROM members WHERE user_id = ?), 0) + 1)",
                      (msg["user"], msg["user"], msg["user"]))
    conn.commit()
    conn.close()
