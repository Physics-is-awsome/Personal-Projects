from slack_sdk import WebClient
import sqlite3
from datetime import datetime, timedelta
import os

SLACK_TOKEN = os.getenv("SLACK_TOKEN")
if not SLACK_TOKEN:
    raise ValueError("SLACK_TOKEN not set")
client = WebClient(token=SLACK_TOKEN)

YOUR_USER_ID = "U08046ZTJSZ"  # Replace with your Slack user ID

def init_db():
    conn = sqlite3.connect("engagement.db")
    c = conn.cursor()
    c.execute('''CREATE TABLE IF NOT EXISTS messages 
                 (timestamp TEXT, user_id TEXT, channel_id TEXT, content TEXT)''')
    c.execute('''CREATE TABLE IF NOT EXISTS members 
                 (user_id TEXT PRIMARY KEY, region TEXT, message_count INTEGER, points INTEGER)''')
    conn.commit()
    conn.close()

def send_dm_summary(user_id, channel_id, message_count, active_users):
    try:
        # Open a DM channel with the user
        response = client.conversations_open(users=[user_id])
        if not response["ok"]:
            print(f"Error opening DM: {response['error']}")
            return
        dm_channel = response["channel"]["id"]
        
        # Send the summary
        message = (f"📊 Message Fetch Summary for <#{channel_id}>\n"
                   f"Fetched {message_count} messages from the last 30 days.\n"
                   f"Active users: {active_users}\n"
                   f"Data stored in engagement.db for the dashboard.")
        client.chat_postMessage(channel=dm_channel, text=message)
        print("DM sent successfully!")
    except Exception as e:
        print(f"Error sending DM: {e}")

def fetch_messages(channel_id, days=30):
    cutoff = (datetime.now() - timedelta(days=days)).timestamp()
    response = client.conversations_history(channel=channel_id, oldest=cutoff)
    if not response["ok"]:
        raise Exception(f"API Error: {response['error']}")
    messages = response["messages"]
    
    conn = sqlite3.connect("engagement.db")
    c = conn.cursor()
    
    message_count = 0
    active_users = set()
    
    for msg in messages:
        c.execute("SELECT COUNT(*) FROM messages WHERE timestamp = ?", (msg["ts"],))
        if c.fetchone()[0] == 0:
            c.execute("INSERT INTO messages VALUES (?, ?, ?, ?)",
                      (msg["ts"], msg["user"], channel_id, msg.get("text", "")))
            c.execute("""INSERT OR REPLACE INTO members (user_id, region, message_count, points)
                         VALUES (?, 'Unknown', COALESCE((SELECT message_count FROM members WHERE user_id = ?), 0) + 1,
                         COALESCE((SELECT points FROM members WHERE user_id = ?), 0) + 1)""",
                      (msg["user"], msg["user"], msg["user"]))
            message_count += 1
            active_users.add(msg["user"])
    
    conn.commit()
    conn.close()
    
    # Send summary to your DM
    send_dm_summary(YOUR_USER_ID, channel_id, message_count, len(active_users))
    
    return message_count, active_users

if __name__ == "__main__":
    init_db()
    channel_id = "C0883QT1Y84"  # Replace with your channel ID
    message_count, active_users = fetch_messages(channel_id)
    print(f"Fetched {message_count} messages, {len(active_users)} active users")
