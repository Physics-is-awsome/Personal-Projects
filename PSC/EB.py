from slack_sdk import WebClient
import os

SLACK_TOKEN = os.getenv("SLACK_TOKEN", "your-slack-bot-token")
client = WebClient(token=SLACK_TOKEN)

response = client.conversations_list()
for channel in response["channels"]:
    print(f"Channel: {channel['name']}, ID: {channel['id']}")
