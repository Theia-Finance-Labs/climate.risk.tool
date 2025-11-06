#!/usr/bin/env python3
"""
Correct encoding fix: Fix multiple encoding corruption patterns found in the CSV.
"""

import pandas as pd
import os
import re
from unidecode import unidecode

CSV_PATH = "workspace/demo_inputs/precomputed_adm_hazards.csv"


def fix_encoding_patterns(text):
    """
    Fix common encoding corruption patterns found in the CSV.
    Handles patterns like: APSo -> ão, AC/ndia -> ândia, A! -> á, etc.
    """
    if not isinstance(text, str):
        return text

    # Pattern-based replacements for common corruptions
    # These are based on the actual corruption patterns found in the file
    # Order matters: more specific patterns first
    replacements = [
        # Specific multi-character patterns first
        (r"AC/ndia", "ândia"),  # AC/ndia -> ândia (e.g., BrasilAC/ndia -> Brasilândia)
        (r"APSes\b", "ães"),  # APSes -> ães (e.g., GuimarAPSes -> Guimarães)
        (r"ASSosa", "çosa"),  # ASSosa -> çosa (e.g., ViASSosa -> Viçosa)
        (r"ASSu", "çu"),  # ASSu -> çu (e.g., IguaASSu -> Iguaçu)
        (r"APSo\b", "ão"),  # APSo -> ão (e.g., EspigAPSo -> Espigão)
        (r"APSo", "ão"),  # General APSo -> ão (fallback for non-word-boundary cases)
        (r"A\(c\)", "é"),  # A(c) -> é (e.g., MA(c)dici -> Médici)
        # Single character corruptions - be careful with context
        (r"A!", "á"),  # A! -> á (e.g., Ji-ParanA! -> Ji-Paraná)
        # A3 patterns - can be ã or ó depending on context
        (
            r"([a-zA-Z])A3([a-zA-Z])",
            r"\1ã\2",
        ),  # A3 -> ã when between letters (e.g., VianA3polis -> Vianãpolis)
        (
            r"([a-zA-Z])A3\b",
            r"\1ó",
        ),  # A3 at end -> ó (e.g., ChapecA3 -> Chapecó, PirapA3 -> Pirapó)
        # Aa patterns
        (r"([a-zA-Z])Aa\b", r"\1ã"),  # Aa -> ã at end of word (e.g., VerAa -> Verã)
        # Additional patterns for remaining corruptions
        (
            r"([a-zA-Z])Aas\b",
            r"\1ês",
        ),  # Aas at end -> ês (e.g., InAas -> Inês, TrAas -> Três)
        (
            r"([a-zA-Z])Aago\b",
            r"\1êgo",
        ),  # Aago at end -> êgo (e.g., SossAago -> Sossêgo)
        (r"([a-zA-Z])Aaco\b", r"\1aço"),  # Aaco at end -> aço (e.g., CirAaco -> Ciraço)
        (
            r"([a-zA-Z])Aamaco\b",
            r"\1êmaco",
        ),  # Aamaco -> êmaco (e.g., TelAamaco -> Telêmaco)
        # More patterns for remaining corruptions
        (
            r"([a-zA-Z])Aancia\b",
            r"\1ência",
        ),  # Aancia -> ência (e.g., QuerAancia -> Querência, InocAancia -> Inocência)
        (r"([a-zA-Z])Aanio\b", r"\1ênio"),  # Aanio -> ênio (e.g., EugAanio -> Eugênio)
        (
            r"([a-zA-Z])Aasine\b",
            r"\1êsine",
        ),  # Aasine -> êsine (e.g., PolAasine -> Polêsine)
        (r"([a-zA-Z])Aaa\b", r"\1êa"),  # Aaa -> êa (e.g., CorrAaa -> Corrêa)
        (
            r"([a-zA-Z])Aania\b",
            r"\1ênia",
        ),  # Aania -> ênia (e.g., EfigAania -> Efigênia)
        (
            r"([a-zA-Z])Aan\b",
            r"\1ão",
        ),  # Aan -> ão (e.g., PiAan -> Pião) - but might need context
        # Final patterns for remaining corruptions
        (r"([a-zA-Z])Aanix\b", r"\1ênix"),  # Aanix -> ênix (e.g., FAanix -> Fênix)
        (
            r"([a-zA-Z])Aancias\b",
            r"\1ências",
        ),  # Aancias -> ências (e.g., PendAancias -> Pendências)
        (
            r"([a-zA-Z])Aancio\b",
            r"\1êncio",
        ),  # Aancio -> êncio (e.g., InocAancio -> Inocêncio)
        (r"([a-zA-Z])Aaca\b", r"\1êca"),  # Aaca -> êca (e.g., SAaca -> Sêca)
        # Fix Aavao pattern - ensure it matches correctly (remove word boundary for better matching)
        (r"([a-zA-Z])Aavao", r"\1êvão"),  # Aavao -> êvão (e.g., EstAavao -> Estêvão)
    ]

    # Apply replacements
    fixed = text
    for pattern, replacement in replacements:
        fixed = re.sub(pattern, replacement, fixed)

    # If still contains corrupted patterns, try unidecode as fallback
    # But first check if it's actually corrupted
    corrupted_indicators = [
        "APSo",
        "AC/ndia",
        "A!",
        "ASSu",
        "A(c)",
        "A3",
        "Aa",
        "APSes",
    ]
    if any(indicator in fixed for indicator in corrupted_indicators):
        # Try unidecode as a fallback
        fixed = unidecode(fixed)

    return fixed


def create_fix_mapping_from_csv():
    """Create mapping by fixing the corrupted names in the CSV"""
    print("Loading CSV to analyze corrupted names...")

    # Load CSV - handle BOM if present
    try:
        df = pd.read_csv(CSV_PATH, low_memory=False, encoding="utf-8-sig")
    except:
        df = pd.read_csv(CSV_PATH, low_memory=False)

    regions = set(df["region"].unique())
    print(f"Found {len(regions)} unique regions in CSV")

    # Find regions with encoding issues
    corrupted_indicators = [
        "APSo",
        "AC/ndia",
        "A!",
        "ASSu",
        "A(c)",
        "A3",
        "Aa",
        "APSes",
        "Ã¡",
        "Ã£",
        "Ã¢",
        "Ã©",
        "Ã­",
        "Ã³",
        "Ãº",
        "Ã§",
        "Ã ",
    ]
    corrupted_regions = []
    for region in regions:
        if any(indicator in region for indicator in corrupted_indicators):
            corrupted_regions.append(region)

    print(f"Found {len(corrupted_regions)} corrupted regions in CSV")

    # Create mapping using pattern-based fixes
    fix_mapping = {}
    for corrupted in corrupted_regions:
        corrected = fix_encoding_patterns(corrupted)
        if corrected != corrupted:  # Only add if it's actually different
            fix_mapping[corrupted] = corrected

    print(f"Created {len(fix_mapping)} corrections")

    # Show examples
    print("\nExamples of corrections:")
    for i, (corrupted, corrected) in enumerate(list(fix_mapping.items())[:20]):
        print(f"  '{corrupted}' -> '{corrected}'")
    if len(fix_mapping) > 20:
        print(f"  ... and {len(fix_mapping) - 20} more")

    return fix_mapping


def apply_fix_to_csv(csv_path, fix_mapping):
    """Apply the fix to the CSV file"""
    print(f"Loading CSV: {csv_path}")

    # Load CSV with low_memory=False to avoid dtype warnings
    # Handle BOM if present
    try:
        df = pd.read_csv(csv_path, low_memory=False, encoding="utf-8-sig")
    except:
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
        try:
            df_check = pd.read_csv(csv_path, low_memory=False, encoding="utf-8-sig")
        except:
            df_check = pd.read_csv(csv_path, low_memory=False)
        regions_after = set(df_check["region"].unique())
        corrupted_indicators = [
            "APSo",
            "AC/ndia",
            "A!",
            "ASSu",
            "A(c)",
            "A3",
            "Aa",
            "APSes",
            "Ã¡",
            "Ã£",
            "Ã¢",
            "Ã©",
            "Ã­",
            "Ã³",
            "Ãº",
            "Ã§",
            "Ã ",
        ]
        still_corrupted = [
            r
            for r in regions_after
            if any(indicator in r for indicator in corrupted_indicators)
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
    print("Strategy: Pattern-based replacement for encoding corruptions")

    # Create fix mapping from CSV
    fix_mapping = create_fix_mapping_from_csv()

    # Apply fix to CSV
    apply_fix_to_csv(CSV_PATH, fix_mapping)

    print("=== Done ===")


if __name__ == "__main__":
    main()
