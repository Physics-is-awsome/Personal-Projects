from slack_sdk import WebClient
import sqlite3
from datetime import datetime, timedelta
import os

SLACK_TOKEN = os.getenv("SLACK_TOKEN")
if not SLACK_TOKEN:
    raise ValueError("SLACK_TOKEN not set")
client = WebClient(token=SLACK_TOKEN)

def init_db():
    conn = sqlite3.connect("engagement.db")
    c = conn.cursor()
    c.execute('''CREATE TABLE IF NOT EXISTS messages 
                 (timestamp TEXT, user_id TEXT, channel_id TEXT, content TEXT)''')
    c.execute('''CREATE TABLE IF NOT EXISTS members 
                 (user_id TEXT PRIMARY KEY, region TEXT, message_count INTEGER, points INTEGER)''')
    conn.commit()
    conn.close()

def fetch_messages(channel_id, days=30):
    cutoff = (datetime.now() - timedelta(days=days)).timestamp()
    response = client.conversations_history(channel=channel_id, oldest=cutoff)
    if not response["ok"]:
        raise Exception(f"API Error: {response['error']}")
    messages = response["messages"]
    
    conn = sqlite3.connect("engagement.db")
    c = conn.cursor()
    for msg in messages:
        c.execute("SELECT COUNT(*) FROM messages WHERE timestamp = ?", (msg["ts"],))
        if c.fetchone()[0] == 0:
            c.execute("INSERT INTO messages VALUES (?, ?, ?, ?)",
                      (msg["ts"], msg["user"], channel_id, msg.get("text", "")))
            c.execute("""INSERT OR REPLACE INTO members (user_id, region, message_count, points)
                         VALUES (?, 'Unknown', COALESCE((SELECT message_count FROM members WHERE user_id = ?), 0) + 1,
                         COALESCE((SELECT points FROM members WHERE user_id = ?), 0) + 1)""",
                      (msg["user"], msg["user"], msg["user"]))
    conn.commit()
    conn.close()

if __name__ == "__main__":
    init_db()
    fetch_messages("C0883QT1Y84")  # Replace with your channel ID
