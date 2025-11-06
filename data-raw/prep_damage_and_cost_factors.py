#!/usr/bin/env python3
"""
Fix encoding issues in damage_and_cost_factors.csv by unidecoding province names.
This converts special characters (e.g., São Paulo -> Sao Paulo) to ASCII.
"""

import pandas as pd
from unidecode import unidecode

# Read the CSV - handle encoding errors by reading with latin-1 first
try:
    df = pd.read_csv("workspace/source_damage_and_cost_factors.csv", encoding="utf-8")
except UnicodeDecodeError:
    print("UTF-8 read failed, trying latin-1 encoding...")
    df = pd.read_csv("workspace/source_damage_and_cost_factors.csv", encoding="latin-1")

# Unidecode the province column
df["province"] = df["province"].apply(lambda x: unidecode(str(x)) if pd.notna(x) else x)

df.loc[df["hazard_type"] == "flood", "hazard_type"] = "Flood"

df.loc[df["hazard_type"] == "drought", "hazard_type"] = "Drought"
df.loc[df["hazard_indicator"] == "HI_days", "hazard_indicator"] = "HI"

df = df.loc[~((df["hazard_type"] == "Drought") & (df["metric"] != "mean")), :]
df.loc[df["hazard_type"] == "Drought", "metric"] = None

df = df[~df["province"].isin(["Rio Grande do Norte", "Rio de Janeiro", "Brazil"])]

df[df == "-"] = None

# Save back to the same file
df.to_csv("tests/tests_data/damage_and_cost_factors.csv", index=False)

print("Successfully unidecoded province names!")
print("\nSample of unidecoded names:")
print(df[df["province"] != "-"]["province"].unique()[:20])
