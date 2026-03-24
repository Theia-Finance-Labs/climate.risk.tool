#!/usr/bin/env python3
import csv
import math
import sys
from pathlib import Path

import pandas as pd


BASE_DIR = Path("workspace/demo_inputs_refacto")
RESULTS_DIR = Path("workspace/check_continuity/config_refacto")

MAIN_RESULTS = RESULTS_DIR / "asset_results_main.csv"
REFACTO_RESULTS = RESULTS_DIR / "asset_results_refacto__+5%.csv"
COMPANIES_XLSX = BASE_DIR / "user_input" / "company_information.xlsx"


HEAT_CONSTANTS = {
    "L0": 339.2285,
    "K0": 87025023,
    "E0": 43.99034,
    "lnA": 2.398,
    "B1": 0.602,
    "B2": 0.455,
    "B3": 0.147,
}


def parse_share(value):
    if value is None or (isinstance(value, float) and math.isnan(value)):
        return None
    if isinstance(value, (int, float)):
        return float(value)
    value = str(value).strip()
    if value.endswith("%"):
        value = value[:-1]
        return float(value) / 100.0
    return float(value)


def parse_refacto_csv(path):
    with path.open("r", encoding="utf-8") as handle:
        reader = csv.reader(handle)
        header = next(reader)
        expected_cols = len(header)
        rows = []
        for row in reader:
            if len(row) == expected_cols:
                rows.append(row)
                continue
            fixed = []
            idx = 0
            while idx < len(row):
                value = row[idx]
                if value.startswith("c(") and not value.endswith(")"):
                    merged = [value]
                    idx += 1
                    while idx < len(row):
                        merged.append(row[idx])
                        if row[idx].endswith(")"):
                            break
                        idx += 1
                    fixed.append(",".join(merged))
                else:
                    fixed.append(value)
                idx += 1
            if len(fixed) != expected_cols:
                raise ValueError(
                    f"Row length mismatch after fix: expected {expected_cols}, got {len(fixed)}"
                )
            rows.append(fixed)
    return pd.DataFrame(rows, columns=header)


def load_company_financials(path):
    companies = pd.read_excel(path)
    companies = companies.rename(
        columns={
            "Company": "company",
            "Revenues": "company_revenue",
            "Net Profit Margin": "net_profit_margin",
        }
    )
    companies["company"] = companies["company"].astype(str)
    companies["net_profit_margin"] = pd.to_numeric(
        companies["net_profit_margin"], errors="coerce"
    )
    companies["company_revenue"] = pd.to_numeric(
        companies["company_revenue"], errors="coerce"
    )
    return companies[["company", "company_revenue", "net_profit_margin"]]


def safe_numeric(series):
    return pd.to_numeric(series, errors="coerce")


def compute_heat_revenue(revenue, hi, damage_factor):
    weighted_lp_loss = (hi / 365.0) * damage_factor
    L_adjusted = HEAT_CONSTANTS["L0"] * (1 + weighted_lp_loss)
    Y_base = math.exp(
        HEAT_CONSTANTS["lnA"]
        + HEAT_CONSTANTS["B1"] * math.log(HEAT_CONSTANTS["K0"])
        + HEAT_CONSTANTS["B2"] * math.log(HEAT_CONSTANTS["L0"])
        + HEAT_CONSTANTS["B3"] * math.log(HEAT_CONSTANTS["E0"])
    )
    Y_shock = math.exp(
        HEAT_CONSTANTS["lnA"]
        + HEAT_CONSTANTS["B1"] * math.log(HEAT_CONSTANTS["K0"])
        + HEAT_CONSTANTS["B2"] * math.log(L_adjusted)
        + HEAT_CONSTANTS["B3"] * math.log(HEAT_CONSTANTS["E0"])
    )
    change = (Y_shock / Y_base) - 1
    return revenue * (1 + change)


def apply_shocks(df):
    df = df.copy()

    df["share_of_economic_activity"] = df["share_of_economic_activity"].apply(parse_share)
    df["baseline_revenue"] = df["company_revenue"] * df["share_of_economic_activity"]
    df["baseline_profit"] = df["baseline_revenue"] * df["net_profit_margin"]

    df["damage_factor"] = safe_numeric(df.get("damage_factor"))
    df["cost_factor"] = safe_numeric(df.get("cost_factor"))
    df["business_disruption"] = safe_numeric(df.get("business_disruption"))
    df["days_danger_total"] = safe_numeric(df.get("days_danger_total"))
    df["land_cover_risk"] = safe_numeric(df.get("land_cover_risk"))
    df["hi"] = safe_numeric(df.get("hi"))
    df["spi3"] = safe_numeric(df.get("spi3"))
    df["flood_depth_cm"] = safe_numeric(df.get("flood_depth_cm"))
    df["fwi"] = safe_numeric(df.get("fwi"))
    df["hazard_intensity"] = safe_numeric(df.get("hazard_intensity"))

    # Backfill indicator-specific intensities from the generic hazard_intensity column
    if "hazard_intensity" in df.columns:
        heat_mask = (df.get("hazard_type") == "Heat") & df["hi"].isna()
        drought_mask = (df.get("hazard_type") == "Drought") & df["spi3"].isna()
        flood_mask = (df.get("hazard_type") == "Flood") & df["flood_depth_cm"].isna()
        fire_mask = (df.get("hazard_type") == "Fire") & df["fwi"].isna()

        df.loc[heat_mask, "hi"] = df.loc[heat_mask, "hazard_intensity"]
        df.loc[drought_mask, "spi3"] = df.loc[drought_mask, "hazard_intensity"]
        df.loc[flood_mask, "flood_depth_cm"] = df.loc[flood_mask, "hazard_intensity"]
        df.loc[fire_mask, "fwi"] = df.loc[fire_mask, "hazard_intensity"]

    df["revenue_shocked"] = df["baseline_revenue"]
    df["profit_shocked"] = df["baseline_profit"]

    for idx, row in df.iterrows():
        hazard_type = str(row.get("hazard_type", ""))
        asset_category = str(row.get("asset_category", ""))
        revenue = row["baseline_revenue"]
        profit = row["baseline_profit"]

        if pd.isna(revenue) or pd.isna(profit):
            continue

        revenue_shocked = revenue
        profit_base = profit

        if hazard_type == "Flood":
            if asset_category == "agriculture":
                revenue_shocked = revenue * (
                    1 - (row["damage_factor"] or 0)
                ) * (1 - (row["business_disruption"] or 0) / 365.0)
            else:
                revenue_shocked = revenue * (1 - (row["business_disruption"] or 0) / 365.0)
        elif hazard_type == "Drought" and asset_category == "agriculture":
            revenue_shocked = revenue * (1 - (row["damage_factor"] or 0))
        elif hazard_type == "Heat":
            if not pd.isna(row["hi"]) and not pd.isna(row["damage_factor"]):
                revenue_shocked = compute_heat_revenue(revenue, row["hi"], row["damage_factor"])
        elif hazard_type == "Fire" and asset_category == "agriculture":
            revenue_shocked = revenue * (
                1 - (row["land_cover_risk"] or 0)
                * (row["damage_factor"] or 0)
                * ((row["days_danger_total"] or 0) / 365.0)
            )

        profit_base = revenue_shocked * row["net_profit_margin"]
        profit_shocked = profit_base

        if hazard_type == "Flood" and asset_category in ("commercial building", "industrial building"):
            profit_shocked = profit_base - (row["damage_factor"] or 0) * (row["cost_factor"] or 0)
        elif hazard_type == "Fire" and asset_category in ("commercial building", "industrial building"):
            profit_shocked = profit_base - (
                (row["land_cover_risk"] or 0)
                * (row["damage_factor"] or 0)
                * ((row["days_danger_total"] or 0) / 365.0)
                * (row["cost_factor"] or 0)
            )

        df.at[idx, "revenue_shocked"] = revenue_shocked
        df.at[idx, "profit_shocked"] = profit_shocked

    df["revenue_loss"] = df["baseline_revenue"] - df["revenue_shocked"]
    df["profit_loss"] = (df["baseline_revenue"] * df["net_profit_margin"]) - df["profit_shocked"]
    return df


def normalize_key_fields(df):
    df = df.copy()

    if "hazard_return_period" in df.columns:
        df["hazard_return_period"] = pd.to_numeric(
            df["hazard_return_period"], errors="coerce"
        )

    for col in ["scenario_name", "season"]:
        if col in df.columns:
            df[col] = df[col].replace({"NA": None, "": None})

    if "event_year" in df.columns:
        df["event_year"] = pd.to_numeric(df["event_year"], errors="coerce")

    return df


def build_join_key(df):
    df = normalize_key_fields(df)
    columns = [
        "asset",
        "company",
        "hazard_type",
        "event_year",
        "hazard_return_period",
        "scenario_name",
        "season",
    ]
    for col in columns:
        if col not in df.columns:
            df[col] = None
    df["join_key"] = df[columns].fillna("").astype(str).agg("|".join, axis=1)
    return df


def summarize_deltas(merged, threshold=1e-3):
    merged["revenue_loss_delta"] = merged["revenue_loss_refacto"] - merged["revenue_loss_main"]
    merged["profit_loss_delta"] = merged["profit_loss_refacto"] - merged["profit_loss_main"]

    revenue_diff = merged[merged["revenue_loss_delta"].abs() > threshold]
    profit_diff = merged[merged["profit_loss_delta"].abs() > threshold]

    return revenue_diff, profit_diff


def main():
    main_df = pd.read_csv(MAIN_RESULTS)
    refacto_df = parse_refacto_csv(REFACTO_RESULTS)

    companies = load_company_financials(COMPANIES_XLSX)

    for df in (main_df, refacto_df):
        df.rename(columns={"Company": "company", "Asset": "asset"}, inplace=True)

    main_df = main_df.merge(companies, on="company", how="left")
    refacto_df = refacto_df.merge(companies, on="company", how="left")

    main_df = apply_shocks(main_df)
    refacto_df = apply_shocks(refacto_df)

    main_df = build_join_key(main_df)
    refacto_df = build_join_key(refacto_df)

    merged = main_df.merge(
        refacto_df,
        on="join_key",
        how="inner",
        suffixes=("_main", "_refacto"),
    )

    revenue_diff, profit_diff = summarize_deltas(merged)

    print(f"Matched rows: {len(merged)}")
    print(f"Revenue diffs (>0.001): {len(revenue_diff)}")
    print(f"Profit diffs (>0.001): {len(profit_diff)}")

    if len(revenue_diff) > 0:
        top_rev = revenue_diff.reindex(
            revenue_diff["revenue_loss_delta"].abs().sort_values(ascending=False).index
        ).head(10)
        print("\nTop 10 revenue loss deltas:")
        print(
            top_rev[
                [
                    "asset_main",
                    "hazard_type_main",
                    "event_year_main",
                    "scenario_name_main",
                    "hazard_name_main",
                    "hazard_name_refacto",
                    "revenue_loss_main",
                    "revenue_loss_refacto",
                    "revenue_loss_delta",
                ]
            ].to_string(index=False)
        )
        print("\nRevenue delta hazard counts:")
        print(revenue_diff["hazard_type_main"].value_counts().to_string())

    if len(profit_diff) > 0:
        top_profit = profit_diff.reindex(
            profit_diff["profit_loss_delta"].abs().sort_values(ascending=False).index
        ).head(10)
        print("\nTop 10 profit loss deltas:")
        print(
            top_profit[
                [
                    "asset_main",
                    "hazard_type_main",
                    "event_year_main",
                    "scenario_name_main",
                    "hazard_name_main",
                    "hazard_name_refacto",
                    "profit_loss_main",
                    "profit_loss_refacto",
                    "profit_loss_delta",
                ]
            ].to_string(index=False)
        )
        print("\nProfit delta hazard counts:")
        print(profit_diff["hazard_type_main"].value_counts().to_string())

    if len(revenue_diff) > 0 or len(profit_diff) > 0:
        focus_rows = pd.concat([revenue_diff, profit_diff]).drop_duplicates(subset=["join_key"])
        focus_cols = [
            "asset_main",
            "company_main",
            "hazard_type_main",
            "event_year_main",
            "scenario_name_main",
            "season_main",
            "hazard_return_period_main",
            "share_of_economic_activity_main",
            "share_of_economic_activity_refacto",
            "damage_factor_main",
            "damage_factor_refacto",
            "business_disruption_main",
            "business_disruption_refacto",
            "cost_factor_main",
            "cost_factor_refacto",
            "land_cover_risk_main",
            "land_cover_risk_refacto",
            "days_danger_total_main",
            "days_danger_total_refacto",
            "spi3_main",
            "spi3_refacto",
            "hi_main",
            "hi_refacto",
            "flood_depth_cm_refacto",
            "hazard_intensity_main",
            "revenue_loss_main",
            "revenue_loss_refacto",
            "profit_loss_main",
            "profit_loss_refacto",
        ]
        focus_cols = [col for col in focus_cols if col in focus_rows.columns]

        def derive_intensity(row, suffix):
            hazard_type = row.get(f"hazard_type_{suffix}")
            if hazard_type == "Flood":
                return row.get(f"hazard_intensity_{suffix}") or row.get(f"flood_depth_cm_{suffix}")
            if hazard_type == "Drought":
                return row.get(f"spi3_{suffix}")
            if hazard_type == "Heat":
                return row.get(f"hi_{suffix}") or row.get(f"hazard_intensity_{suffix}")
            if hazard_type == "Fire":
                return row.get(f"fwi_{suffix}") or row.get(f"hazard_intensity_{suffix}")
            return None

        focus_rows = focus_rows.copy()
        focus_rows["intensity_main"] = focus_rows.apply(lambda r: derive_intensity(r, "main"), axis=1)
        focus_rows["intensity_refacto"] = focus_rows.apply(lambda r: derive_intensity(r, "refacto"), axis=1)
        focus_cols.extend(["intensity_main", "intensity_refacto"])

        print("\nFactor deltas for mismatched rows:")
        print(focus_rows[focus_cols].head(15).to_string(index=False))

        print("\nAll mismatched asset-hazard rows (key fields):")
        key_cols = [
            "asset_main",
            "company_main",
            "hazard_type_main",
            "event_year_main",
            "scenario_name_main",
            "season_main",
            "hazard_return_period_main",
            "intensity_main",
            "intensity_refacto",
            "damage_factor_main",
            "damage_factor_refacto",
            "business_disruption_main",
            "business_disruption_refacto",
            "revenue_loss_main",
            "revenue_loss_refacto",
            "profit_loss_main",
            "profit_loss_refacto",
        ]
        key_cols = [col for col in key_cols if col in focus_rows.columns]
        print(focus_rows[key_cols].to_string(index=False))

    print("\nDetailed comparison is available in memory; run with a CSV export if needed.")


if __name__ == "__main__":
    main()
