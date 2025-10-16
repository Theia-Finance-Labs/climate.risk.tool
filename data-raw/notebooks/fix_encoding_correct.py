#!/usr/bin/env python3
"""
Correct encoding fix: Reverse the latin1->utf8 corruption and then unidecode.
"""

import pandas as pd
import os
from unidecode import unidecode

CSV_PATH = "data-raw/workspace/precomputed_adm_hazards.csv"


def reverse_corruption_and_unidecode(corrupted_name):
    """
    Reverse the latin1->utf8 corruption and then unidecode.
    """
    try:
        # Reverse the corruption: encode as latin1, decode as utf-8
        original_name = corrupted_name.encode("latin1").decode("utf-8")
        # Now unidecode the original name
        unidecoded_name = unidecode(original_name)
        return unidecoded_name
    except (UnicodeEncodeError, UnicodeDecodeError):
        # If that fails, try direct unidecode
        return unidecode(corrupted_name)


def create_fix_mapping_from_csv():
    """Create mapping by fixing the corrupted names in the CSV"""
    print("Loading CSV to analyze corrupted names...")

    # Load CSV
    df = pd.read_csv(CSV_PATH, low_memory=False)
    regions = set(df["region"].unique())

    # Find regions with encoding issues
    corrupted_regions = []
    for region in regions:
        if any(
            char in region
            for char in ["Ã¡", "Ã£", "Ã¢", "Ã©", "Ã­", "Ã³", "Ãº", "Ã§", "Ã "]
        ):
            corrupted_regions.append(region)

    print(f"Found {len(corrupted_regions)} corrupted regions in CSV")

    # Create mapping using proper corruption reversal + unidecode
    fix_mapping = {}
    for corrupted in corrupted_regions:
        corrected = reverse_corruption_and_unidecode(corrupted)
        if corrected != corrupted:  # Only add if it's actually different
            fix_mapping[corrupted] = corrected

    print(f"Created {len(fix_mapping)} corrections")

    # Show examples
    print("Examples of corrections:")
    for i, (corrupted, corrected) in enumerate(list(fix_mapping.items())[:15]):
        print(f"  '{corrupted}' -> '{corrected}'")
    if len(fix_mapping) > 15:
        print(f"  ... and {len(fix_mapping) - 15} more")

    return fix_mapping


def apply_fix_to_csv(csv_path, fix_mapping):
    """Apply the fix to the CSV file"""
    print(f"Loading CSV: {csv_path}")

    # Load CSV with low_memory=False to avoid dtype warnings
    df = pd.read_csv(csv_path, low_memory=False)
    print(f"CSV has {len(df)} rows")

    # Check how many regions need fixing
    regions_to_fix = set(fix_mapping.keys())
    csv_regions = set(df["region"].unique())

    regions_in_csv = regions_to_fix.intersection(csv_regions)
    print(f"Found {len(regions_in_csv)} corrupted regions in CSV that need fixing")

    # Show some examples
    if regions_in_csv:
        print("Examples of regions to be fixed:")
        for i, region in enumerate(sorted(list(regions_in_csv))[:10]):
            print(f"  '{region}' -> '{fix_mapping[region]}'")
        if len(regions_in_csv) > 10:
            print(f"  ... and {len(regions_in_csv) - 10} more")

    # Apply the fixes
    if regions_in_csv:
        print("Applying fixes...")
        df["region"] = df["region"].map(fix_mapping).fillna(df["region"])

        # Save the fixed version
        output_path = csv_path.replace(".csv", "_fixed.csv")
        df.to_csv(output_path, index=False)
        print(f"Saved fixed CSV to: {output_path}")

        # Create backup and replace original
        backup_path = csv_path.replace(".csv", "_backup.csv")

        if os.path.exists(csv_path):
            os.rename(csv_path, backup_path)
            print(f"Backed up original to: {backup_path}")

        os.rename(output_path, csv_path)
        print(f"Replaced original with fixed version")

        # Verify the fix
        print("Verifying the fix...")
        df_check = pd.read_csv(csv_path, low_memory=False)
        regions_after = set(df_check["region"].unique())
        still_corrupted = [
            r
            for r in regions_after
            if any(
                char in r
                for char in ["Ã¡", "Ã£", "Ã¢", "Ã©", "Ã­", "Ã³", "Ãº", "Ã§", "Ã "]
            )
        ]
        print(f"Regions still corrupted after fix: {len(still_corrupted)}")

        if len(still_corrupted) == 0:
            print("✅ SUCCESS: All encoding issues have been fixed!")
        else:
            print("❌ Some regions are still corrupted:")
            for region in still_corrupted[:10]:
                print(f"  {region}")
            if len(still_corrupted) > 10:
                print(f"  ... and {len(still_corrupted) - 10} more")

        print("\nSample of fixed region names:")
        fixed_regions = sorted(list(regions_after))
        for i, region in enumerate(fixed_regions[:15]):
            print(f"  {region}")

        return df
    else:
        print("No corrupted regions found in CSV - nothing to fix")
        return df


def main():
    """Main function"""
    print("=== Correct Encoding Fix ===")
    print("Strategy: Reverse latin1->utf8 corruption, then unidecode")

    # Create fix mapping from CSV
    fix_mapping = create_fix_mapping_from_csv()

    # Apply fix to CSV
    apply_fix_to_csv(CSV_PATH, fix_mapping)

    print("=== Done ===")


if __name__ == "__main__":
    main()
