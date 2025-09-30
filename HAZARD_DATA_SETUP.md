# Hazard Data Setup Guide for Developers

This guide explains how to set up hazard data in the Climate Risk Tool workspace and run the data processing pipeline to generate Brazil-specific subsets.

## Overview

The Climate Risk Tool processes global hazard maps to create Brazil-specific subsets for risk analysis. The workflow involves:

1. **Setting up the workspace structure** with global hazard data
2. **Running the data processing script** to extract Brazil-specific subsets
3. **Using the processed data** in the application

## Workspace Structure

### Required Directory Structure

Create the following directory structure in your workspace:

```
workspace/
├── hazards_world/                    # Global hazard data (source)
│   ├── flood/                        # Flood hazard type
│   │   ├── scenario1.tif            # Scenario files (e.g., rcp85_h100glob.tif)
│   │   ├── scenario2.tif            # Additional scenarios
│   │   └── ...
│   ├── heat/                         # Heat hazard type
│   │   ├── scenario1.tif
│   │   └── ...
│   ├── drought/                      # Drought hazard type
│   │   ├── scenario1.tif
│   │   └── ...
│   └── ...                           # Other hazard types
├── Brazil Borders/                   # Administrative boundaries
│   ├── geoBoundaries-BRA-ADM1-all/   # State-level boundaries
│   └── geoBoundaries-BRA-ADM2-all/   # Municipality-level boundaries
└── Flood Damage and Cost Factors/    # Damage and cost factors
    └── damage_and_cost_factors.csv
```

### Hazard Data Organization Rules

**Critical**: Follow this exact structure for the hazard data to work correctly:

1. **Hazard Type Folders**: Each hazard type (flood, heat, drought, etc.) must be in its own subfolder under `hazards_world/`
2. **Scenario Files**: Each scenario file goes directly in the hazard type folder
3. **File Naming**: Use descriptive scenario names (e.g., `rcp85_h100glob.tif`, `rcp45_h50glob.tif`)
4. **File Format**: All hazard files must be `.tif` raster files

#### Example Structure:
```
workspace/hazards_world/
├── flood/
│   ├── global_rcp85_h100glob.tif     # RCP8.5, 100-year flood
│   ├── global_rcp85_h10glob.tif      # RCP8.5, 10-year flood
│   ├── global_rcp45_h100glob.tif     # RCP4.5, 100-year flood
│   └── global_rcp45_h10glob.tif      # RCP4.5, 10-year flood
├── heat/
│   ├── global_rcp85_temp_high.tif    # RCP8.5, high temperature
│   └── global_rcp45_temp_high.tif    # RCP4.5, high temperature
└── drought/
    ├── global_rcp85_drought_severe.tif
    └── global_rcp45_drought_severe.tif
```

## Data Processing Pipeline

### Step 1: Run the Brazil Extraction Script

The main data processing script is located at `data-raw/process_flood_maps_brazil.R`. This script:

1. **Reads global hazard maps** from `workspace/hazards_world/`
2. **Loads Brazil administrative boundaries** from `workspace/Brazil Borders/`
3. **Extracts Brazil-specific subsets** using geospatial clipping
4. **Saves processed files** to `tests/tests_data/hazards/`

#### Running the Script

```bash
# Navigate to the project root
cd /path/to/climate.risk.tool

# Run the processing script
Rscript data-raw/process_flood_maps_brazil.R
```

#### What the Script Does

1. **Validates Input Structure**:
   - Checks that `workspace/hazards_world/` exists
   - Verifies Brazil boundaries are available
   - Lists all `.tif` files found

2. **Processes Each Hazard File**:
   - Loads the global raster
   - Crops to Brazil's bounding box (for efficiency)
   - Masks using Brazil's administrative boundaries
   - Saves as `{scenario}_brazil.tif`

3. **Maintains Directory Structure**:
   - Preserves hazard type organization
   - Creates corresponding subdirectories in output
   - Example: `flood/global_rcp85_h100glob.tif` → `flood/global_rcp85_h100glob_brazil.tif`

4. **Provides Processing Statistics**:
   - File size reduction percentages
   - Data coverage statistics
   - Processing success/failure status

### Step 2: Verify Output

After running the script, check the output in `tests/tests_data/hazards/`:

```
tests/tests_data/hazards/
├── flood/
│   ├── global_rcp85_h100glob_brazil.tif
│   ├── global_rcp85_h10glob_brazil.tif
│   └── ...
├── heat/
│   ├── global_rcp85_temp_high_brazil.tif
│   └── ...
└── ...
```

## Application Integration

### How the Application Uses Hazard Data

The Climate Risk Tool loads hazard data through the `load_hazards()` function:

1. **Automatic Discovery**: Scans `tests/tests_data/hazards/` recursively
2. **Hazard Type Detection**: Uses folder names as hazard types
3. **Scenario Naming**: Uses filenames (minus extension) as scenario names
4. **Naming Convention**: Creates keys as `{hazard_type}__{scenario}`

#### Example Loaded Hazards:
```r
hazards <- load_hazards("tests/tests_data/hazards/")
# Results in:
# hazards$flood__global_rcp85_h100glob
# hazards$flood__global_rcp85_h10glob
# hazards$heat__global_rcp85_temp_high
```

### Performance Optimization

The `load_hazards()` function includes automatic aggregation for performance:

- **Default Aggregation Factor**: 16x (reduces file size and processing time)
- **Caching**: Aggregated files are cached with `__agg16` suffix
- **Memory Management**: Configurable memory usage for large datasets

## Troubleshooting

### Common Issues

1. **"No .tif files found"**:
   - Verify files are in `.tif` format
   - Check that files are in hazard type subfolders
   - Ensure files are not corrupted

2. **"Brazil boundaries file not found"**:
   - Verify `workspace/Brazil Borders/geoBoundaries-BRA-ADM1-all/` exists
   - Check that `geoBoundaries-BRA-ADM1_simplified.geojson` is present

3. **Processing Errors**:
   - Check file permissions
   - Ensure sufficient disk space
   - Verify raster files are valid GeoTIFF format

4. **Memory Issues**:
   - Reduce aggregation factor in `load_hazards()`
   - Process files individually
   - Increase system memory or use swap

### File Size Considerations

- **Global hazard files**: Can be very large (several GB each)
- **Brazil subsets**: Typically 90%+ smaller than global files
- **Aggregated files**: Additional 95%+ reduction for processing

### Performance Tips

1. **Use Aggregation**: Always use aggregation factor > 1 for development
2. **Cache Aggregated Files**: Enable caching to avoid re-aggregation
3. **Process Incrementally**: Add hazard types gradually during development
4. **Monitor Disk Space**: Ensure sufficient space for processed files

## Development Workflow

### Adding New Hazard Types

1. **Create hazard type folder** in `workspace/hazards_world/`
2. **Add scenario files** to the folder
3. **Run processing script** to generate Brazil subsets
4. **Test in application** using `load_hazards()`

### Adding New Scenarios

1. **Add scenario file** to appropriate hazard type folder
2. **Run processing script** (will process new files)
3. **Verify in application** that new scenarios appear in dropdowns

### Updating Existing Data

1. **Replace scenario files** in `workspace/hazards_world/`
2. **Delete old processed files** in `tests/tests_data/hazards/`
3. **Run processing script** to regenerate subsets
4. **Clear aggregated cache** if needed (delete `__agg*.tif` files)

## Data Sources

### Recommended Data Sources

- **Flood Maps**: Global flood hazard maps from various climate models
- **Heat Maps**: Temperature projections and heat wave data
- **Drought Maps**: Drought severity and frequency projections
- **Other Hazards**: Wildfire, sea level rise, etc.

### Data Requirements

- **Format**: GeoTIFF (.tif) raster files
- **Projection**: Any standard projection (script handles transformation)
- **Resolution**: Higher resolution preferred (aggregation handles performance)
- **Coverage**: Global coverage (script extracts Brazil subset)

## Script Customization

### Modifying the Processing Script

The `data-raw/process_flood_maps_brazil.R` script can be customized for:

1. **Different Countries**: Change boundary files and country union
2. **Different Output Formats**: Modify terra::writeRaster() parameters
3. **Additional Processing**: Add data validation, statistics, or transformations
4. **Batch Processing**: Process multiple countries or regions

### Environment Variables

Set these for different processing modes:

```bash
# Process with higher memory allocation
R_MAX_VSIZE=8G Rscript data-raw/process_flood_maps_brazil.R

# Process with verbose output
R_VERBOSE=TRUE Rscript data-raw/process_flood_maps_brazil.R
```

## Summary

This setup process creates a robust pipeline for:

1. **Organizing global hazard data** in a structured format
2. **Automatically processing** Brazil-specific subsets
3. **Integrating with the application** seamlessly
4. **Maintaining performance** through aggregation and caching

Follow this guide to set up hazard data for the Climate Risk Tool, ensuring proper organization and efficient processing for Brazil-specific climate risk analysis.
