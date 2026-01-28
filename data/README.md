
# Dataset: Flight Perfromance in Proximity to Wildlife Strike Incidents

## Overview
This dataset is a multi-source, integrated snapshot of flight performance in the context of wildlife strike incidents. It is split into two relational files.

* **Primary File:** `flight_data.parquet` (Table with ~2M flight observations)
* **Support File:** `strike_data.parquet` (Table with the associated strike event details)

**Relationship:** Every flight in the primary file is associated with a wildlife strike record in the support file based on proximity (±48-hour window at the strike flight's destination airport).

---

## File 1: `flight_data.parquet` (Flight Observations)
This is the core observation file. Each row represents a flight departure.

### ✈️ Flight & Airline Variables (BTS)
| Variable | Description |
| :--- | :--- |
| `airport` | Origin airport code (IATA). |
| `carrier_code` | Unique airline designator. |
| `tail_number` | Aircraft identification number. |
| `flight_number` | Flight ID number. |
| `date` | Date of departure (YYYY-MM-DD). |
| `scheduled_departure_datetime` | Planned departure timestamp. |
| `destination_airport` | Target airport code (IATA). |
| `scheduled_departure_time` | Planned local time of departure. |
| `actual_departure_time` | Actual local time of departure. |
| `departure_delay_minutes` | Total delay in minutes (Actual - Scheduled). |
| `scheduled_elapsed_time_minutes` | Planned duration of flight. |
| `actual_elapsed_time_minutes` | Actual duration of flight. |
| `wheels_off_time` | Precise timestamp of takeoff. |
| `taxi_out_time_minutes` | Time spent between gate and takeoff. |
| `cancelled` | Binary indicator (1 = Cancelled, 0 = Operated). |
| `delay_carrier_minutes` | Attribution of delay to maintenance, crew, or airline operations. |
| `delay_weather_minutes` | Attribution of delay to significant or extreme weather conditions. |
| `delay_national_aviation_system_minutes` | Attribution of delay to traffic volume, ATC, airport operations. |
| `delay_security_minutes` | Attribution of delay to security breaches or terminal evacuations. |
| `delay_late_aircraft_arrival_minutes` | Attribution of delay to ripple effect from previous delayed flights. |

### 🌦️ Meteorological Variables (Visual Crossing)
| Variable | Description |
| :--- | :--- |
| `temp` / `feelslike` | Temperature and "apparent" temperature. |
| `humidity` / `dew` | Moisture levels and dew point. |
| `precip` / `precipprob` | Precipitation amount and probability (%). |
| `snow` / `snowdepth` | Snowfall amount and accumulation. |
| `windspeed` / `windgust` | Sustained wind and peak gust speed. |
| `visibility` / `cloudcover` | Visibility distance and cloud cover percentage. |
| `solarenergy` / `uvindex` | Solar intensity (MJ/m²) and UV index. |
| `conditions` / `icon` | Text and categorical weather descriptions. |

### 🦅 Wildlife Strike Variables (FAA)
| Variable | Description |
| :--- | :--- |
| `strike_index_nr` | Unique ID from the FAA database. |
| `strike_airport` | Airport where the strike was reported to have occurred. |
| `strike_airline` | Airline involved in the strike event. |
| `strike_date_time` | Timestamp of the wildlife collision, if available. |
| `strike_arrival_departure` | Phase of flight during the strike event (Arrival or Departure). |
| `strike_damage` | Binary indicator (1 = Damage Indicated, 0 = No Damage Indicated). |
---

## File 2: `strike_data.parquet` (FAA Wildlife Strike Database)
This file contains the specific details of the wildlife collision events as recorded in the FAA database. Variable
definitions and descriptions are located in the `strike_metadata.xlsx` file, which is drawn directly from the FAA's
Wildlife Strike Database search tool.

---

## Relationship & Join Logic
The datasets can be joined on the `strike_index_nr` and `index_nr` fields in 
the `flight_data.parquet` and `strike_data.parquet`, respectively. 

---

## Data Sources
* **Flight/On-Time Data:** [Bureau of Transportation Statistics (BTS)](https://www.transtats.bts.gov/ontime/)
* **Weather Data:** [Visual Crossing API](https://www.visualcrossing.com/)
* **Wildlife Strike Data:** [FAA Wildlife Strike Database](https://wildlife.faa.gov/search)
