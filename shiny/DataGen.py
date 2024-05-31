import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import random
import time

# Define the date range
start_date = datetime.now() + timedelta(days=100)
end_date = datetime.now()
date_range = pd.date_range(end_date, start_date, normalize=True)

# Generate sample data
np.random.seed(42)  # For reproducibility
def generate_data(date):
    return {        
        'Date': [date] * 15,  # Repeat date three times
        'X1': [random.randint(1, 10000) for _ in range(15)],  # Generate three random values
        'X2': [random.randint(1, 10000) for _ in range(15)]
    }

while True:
    end_date += timedelta(days=1)  # Increment the date
    
    new_data = generate_data(end_date)
    df = pd.DataFrame(new_data)
    with open('data.csv', 'a') as f:
        df.to_csv(f, header=f.tell()==0, index=False, mode='a')
    print("Data added to data.csv.")
    
    # Wait for 2 seconds
    time.sleep(2)