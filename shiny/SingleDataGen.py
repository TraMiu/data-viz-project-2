import pandas as pd
import csv
import random
import time
import os

# Function to generate new data
def get_new_data():
    new_data = pd.DataFrame({'X1': [random.uniform(-3, 3) for _ in range(5)]})
    return new_data



# Path to the CSV file
csv_file_name = "data.csv"

# Main loop to continuously add new data
while True:
    new_data = get_new_data()
    
    # Append new data to the CSV file
    with open(csv_file_name, 'a') as f:
        new_data.to_csv(f, header=False, index=False, line_terminator='\n')
    
    print("New data added to the file.")
    
    # Pause execution for 5 seconds
    time.sleep(1)