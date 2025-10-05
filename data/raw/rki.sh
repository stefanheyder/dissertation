#!/bin/bash

# Configuration
REMOTE_HOST="maurice"
REMOTE_PATH="media/working_directory/rki"
START_DATE="2020-10-12"
END_DATE="2022-01-17"
INCREMENT_DAYS=7

# Convert dates to epoch seconds for comparison (macOS BSD date syntax)
start_epoch=$(date -j -f "%Y-%m-%d" "$START_DATE" "+%s")
end_epoch=$(date -j -f "%Y-%m-%d" "$END_DATE" "+%s")

# Initialize current date
current_epoch=$start_epoch

echo "Copying RKI COVID-19 files from $REMOTE_HOST:/$REMOTE_PATH"
echo "Date range: $START_DATE to $END_DATE (weekly increments)"
echo "----------------------------------------"

# Counter for progress
file_count=0

# Loop through dates in weekly increments
while [ $current_epoch -le $end_epoch ]; do
    # Convert epoch back to YYYY-MM-DD format (macOS BSD date syntax)
    current_date=$(date -r $current_epoch "+%Y-%m-%d")
    
    # Construct filename
    filename="RKI_COVID19_${current_date}.csv.gz"
    
    # Display what we're copying
    echo "Copying: $filename"
    
    # Copy file using scp
    scp "${REMOTE_HOST}:/${REMOTE_PATH}/${filename}" . 2>/dev/null
    
    # Check if copy was successful
    if [ $? -eq 0 ]; then
        echo "✓ Successfully copied $filename"
    else
        echo "✗ Failed to copy $filename (file may not exist)"
    fi
    
    # Increment counter
    ((file_count++))
    
    # Add 7 days (7 * 24 * 60 * 60 seconds)
    current_epoch=$((current_epoch + INCREMENT_DAYS * 86400))
done

echo "----------------------------------------"
echo "Process completed. Attempted to copy $file_count files."
echo "Files copied to: $(pwd)"
