from nsepy import get_history
import pandas as pd
from datetime import date
import time

# Step 1: Load all NSE symbols
url = "https://www1.nseindia.com/content/equities/EQUITY_L.csv"
headers = {"User-Agent": "Mozilla/5.0"}
df_symbols = pd.read_csv(url, header=0)
symbols = df_symbols["SYMBOL"].dropna().unique().tolist()

# Optional: Limit to first 10 for testing (remove this for full run)
symbols = symbols[:10]

# Step 2: Fetch historical data
start = date(2024, 1, 1)
end = date(2024, 12, 31)

all_data = []

for symbol in symbols:
    try:
        data = get_history(symbol=symbol,
                           start=start,
                           end=end)
        data.reset_index(inplace=True)
        data["Ticker"] = symbol
        all_data.append(data[["Date", "Ticker", "Open", "High", "Low", "Close", "Volume"]])
        print(f"Fetched: {symbol}")
        time.sleep(1)  # Respect NSE rate limit
    except Exception as e:
        print(f"Failed for {symbol}: {e}")

# Step 3: Save to Excel
final_df = pd.concat(all_data)
final_df.to_excel("NSE_OHLCV_Data.xlsx", index=False)
