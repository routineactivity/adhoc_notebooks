import os
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

def aoristic(input_csv=None, input_df=None,
                comm_date_fr='comm_date_fr', comm_date_to='comm_date_to',
                comm_time_fr='comm_time_fr', comm_time_to='comm_time_to',
                output_directory='aoristic_output_eu'):
    """
    Aoristic analysis function for data using YYYY-MM-DD date formats.
    """

    # raise error if input method not provided
    if input_csv is None and input_df is None:
        raise ValueError("Please provide either an input CSV file path or a DataFrame.")

    # read data
    if input_df is not None:
        df = input_df.copy()
    else:
        df = pd.read_csv(input_csv)

    # create output directory if it doesn't exist
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    # data cleaning of missing values
    df.fillna({comm_date_to: df[comm_date_fr], comm_time_to: df[comm_time_fr]}, inplace=True)
    df.dropna(subset=[comm_date_fr, comm_date_to, comm_time_fr, comm_time_to], inplace=True)

    # parse datetimes (assumes data is YYYY-MM-DD HH:MM:SS)
    df['start_date'] = pd.to_datetime(
        df[comm_date_fr].astype(str) + ' ' + df[comm_time_fr].astype(str),
        format='%Y-%m-%d %H:%M:%S',
        errors='coerce'
    )
    df['end_date'] = pd.to_datetime(
        df[comm_date_to].astype(str) + ' ' + df[comm_time_to].astype(str),
        format='%Y-%m-%d %H:%M:%S',
        errors='coerce'
    )

    # drop rows where parse failed (NaT)
    df.dropna(subset=['start_date', 'end_date'], inplace=True)

    # aoristic analysis by day of the week
    daily_totals = np.zeros(7)

    for _, row in df.iterrows():
        start_day = row['start_date'].weekday()
        end_day = row['end_date'].weekday()
        total_days = (row['end_date'] - row['start_date']).days + 1
        total_days = max(total_days, 1)
        value_per_day = 1 / total_days

        for offset in range(total_days):
            day = (start_day + offset) % 7
            daily_totals[day] += value_per_day

    daily_percentages = (daily_totals / daily_totals.sum()) * 100
    days_labels = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']

    plt.figure(figsize=(9,6))
    sns.set_style("whitegrid")
    sns.barplot(x=days_labels, y=daily_percentages, color='#135DD8')
    plt.title("Proportion of offences per day")
    plt.xlabel("Weekday")
    plt.ylabel("% of offences")
    plt.savefig(os.path.join(output_directory, 'aoristic_totals_by_day.png'))
    plt.close()

    # aoristic analysis by hour of day
    hourly_totals = np.zeros(24)

    for _, row in df.iterrows():
        duration = int((row['end_date'] - row['start_date']).total_seconds() / 3600) + 1
        duration = max(duration, 1)
        value_per_hour = 1 / duration

        current_time = row['start_date']
        for _ in range(duration):
            hourly_totals[current_time.hour] += value_per_hour
            current_time += pd.Timedelta(hours=1)

    hourly_percentages = (hourly_totals / hourly_totals.sum()) * 100
    hours_labels = list(range(24))

    plt.figure(figsize=(9,6))
    sns.set_style("whitegrid")
    sns.barplot(x=hours_labels, y=hourly_percentages, color='#135DD8')
    plt.title("Proportion of offences per hour")
    plt.xlabel("Hour of the Day")
    plt.ylabel("% of offences")
    plt.savefig(os.path.join(output_directory, 'aoristic_totals_by_hour.png'))
    plt.close()

    # aoristic heatmap (weekday-hour)
    heatmap_totals = np.zeros((7, 24))

    for _, row in df.iterrows():
        duration = int((row['end_date'] - row['start_date']).total_seconds() / 3600) + 1
        duration = max(duration, 1)
        value_per_hour = 1 / duration

        current_time = row['start_date']
        for _ in range(duration):
            day_idx = current_time.weekday()
            hour_idx = current_time.hour
            heatmap_totals[day_idx, hour_idx] += value_per_hour
            current_time += pd.Timedelta(hours=1)

    heatmap_percentages = (heatmap_totals / heatmap_totals.sum()) * 100

    plt.figure(figsize=(9,6))
    sns.set_style("whitegrid")
    ax = sns.heatmap(heatmap_percentages, cmap='vlag', linewidths=.5, linecolor='white')
    ax.set_yticklabels(days_labels, rotation=0)
    plt.title("Proportion of offences by day and hour")
    plt.xlabel("Hour of Day")
    plt.ylabel("Weekday")
    plt.savefig(os.path.join(output_directory, 'aoristic_heatmap.png'))
    plt.close()

    print(f"Aoristic analysis completed. Files saved in '{output_directory}'.")

def aoristic_us(input_csv=None, input_df=None,
                comm_date_fr='comm_date_fr', comm_date_to='comm_date_to',
                comm_time_fr='comm_time_fr', comm_time_to='comm_time_to',
                output_directory='aoristic_output_us'):
    """
    Aoristic analysis function for data primarily using US formats,
    e.g. mm/dd/yyyy or yyyy-mm-dd, etc., 
    by leveraging more flexible datetime parsing with dayfirst=False.
    """

    # raise error if input method not provided
    if input_csv is None and input_df is None:
        raise ValueError("Please provide either an input CSV file path or a DataFrame.")

    # read data
    if input_df is not None:
        df = input_df.copy()
    else:
        df = pd.read_csv(input_csv)

    # create output directory if it doesn't exist
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    # data cleaning of missing values
    df.fillna({comm_date_to: df[comm_date_fr], comm_time_to: df[comm_time_fr]}, inplace=True)
    df.dropna(subset=[comm_date_fr, comm_date_to, comm_time_fr, comm_time_to], inplace=True)

    # create start and end dates with flexible parsing, dayfirst=False to suit US formats
    df['start_date'] = pd.to_datetime(
        df[comm_date_fr].astype(str) + ' ' + df[comm_time_fr].astype(str),
        #infer_datetime_format=True,
        errors='coerce',
        dayfirst=False
    )
    df['end_date'] = pd.to_datetime(
        df[comm_date_to].astype(str) + ' ' + df[comm_time_to].astype(str),
        #infer_datetime_format=True,
        errors='coerce',
        dayfirst=False
    )

    # aoristic analysis by day of the week
    daily_totals = np.zeros(7)

    for _, row in df.iterrows():
        start_day = row['start_date'].weekday()
        end_day = row['end_date'].weekday()
        total_days = (row['end_date'] - row['start_date']).days + 1
        total_days = max(total_days, 1)
        value_per_day = 1 / total_days

        for offset in range(total_days):
            day = (start_day + offset) % 7
            daily_totals[day] += value_per_day

    daily_percentages = (daily_totals / daily_totals.sum()) * 100
    days_labels = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']

    plt.figure(figsize=(9,6))
    sns.set_style("whitegrid")
    sns.barplot(x=days_labels, y=daily_percentages, color='#135DD8')
    plt.title("Proportion of offences per day")
    plt.xlabel("Weekday")
    plt.ylabel("% of offences")
    plt.savefig(os.path.join(output_directory, 'aoristic_totals_by_day.png'))
    plt.close()

    # aoristic analysis by hour of day
    hourly_totals = np.zeros(24)

    for _, row in df.iterrows():
        duration = int((row['end_date'] - row['start_date']).total_seconds() / 3600) + 1
        duration = max(duration, 1)
        value_per_hour = 1 / duration

        current_time = row['start_date']
        for _ in range(duration):
            hourly_totals[current_time.hour] += value_per_hour
            current_time += pd.Timedelta(hours=1)

    hourly_percentages = (hourly_totals / hourly_totals.sum()) * 100
    hours_labels = list(range(24))

    plt.figure(figsize=(9,6))
    sns.set_style("whitegrid")
    sns.barplot(x=hours_labels, y=hourly_percentages, color='#135DD8')
    plt.title("Proportion of offences per hour")
    plt.xlabel("Hour of the Day")
    plt.ylabel("% of offences")
    plt.savefig(os.path.join(output_directory, 'aoristic_totals_by_hour.png'))
    plt.close()

    # aoristic heatmap (weekday-hour)
    heatmap_totals = np.zeros((7, 24))

    for _, row in df.iterrows():
        duration = int((row['end_date'] - row['start_date']).total_seconds() / 3600) + 1
        duration = max(duration, 1)
        value_per_hour = 1 / duration

        current_time = row['start_date']
        for _ in range(duration):
            day_idx = current_time.weekday()
            hour_idx = current_time.hour
            heatmap_totals[day_idx, hour_idx] += value_per_hour
            current_time += pd.Timedelta(hours=1)

    heatmap_percentages = (heatmap_totals / heatmap_totals.sum()) * 100

    plt.figure(figsize=(9,6))
    sns.set_style("whitegrid")
    ax = sns.heatmap(heatmap_percentages, cmap='vlag', linewidths=.5, linecolor='white')
    ax.set_yticklabels(days_labels, rotation=0)
    plt.title("Proportion of offences by day and hour")
    plt.xlabel("Hour of Day")
    plt.ylabel("Weekday")
    plt.savefig(os.path.join(output_directory, 'aoristic_heatmap.png'))
    plt.close()

    print(f"Aoristic analysis completed. Files saved in '{output_directory}'.")