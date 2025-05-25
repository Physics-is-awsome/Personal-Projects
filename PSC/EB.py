
from slack_sdk import WebClient
import os

SLACK_TOKEN = os.getenv("SLACK_TOKEN", "your-slack-bot-token")
client = WebClient(token=SLACK_TOKEN)

# Test API connection
response = client.api_test()
if response["ok"]:
    print("Slack API connection successful!")
else:
    print(f"Error: {response['error']}")
