# -*- coding: utf-8 -*-
"""
Created on Fri Feb 15 14:06:55 2019

@author: nshanbhag
"""

import pandas as pd

Final_Data_Forecasted_Transfer_Capacities_day_ahead = pd.read_excel (r'C:\Users\mrudrappa\Desktop\Hackaton\hackathon\Final_Data_Forecasted_Transfer_Capacities_day_ahead.xlsx') #for an earlier version of Excel, you may need to use the file extension of 'xls'


Final_Day_Ahead_Prices_Transmission = pd.read_excel (r'C:\Users\mrudrappa\Desktop\Hackaton\hackathon\Final_Day_Ahead_Prices_Transmission.xlsx') #for an earlier version of Excel, you may need to use the file extension of 'xls'


Final_Day_ahead_Actual = pd.read_excel (r'C:\Users\mrudrappa\Desktop\Hackaton\hackathon\Final_Day_ahead_Actual.xlsx') #for an earlier version of Excel, you may need to use the file extension of 'xls'


Entsoe_Final1 = pd.merge(Final_Data_Forecasted_Transfer_Capacities_day_ahead,Final_Day_Ahead_Prices_Transmission,
                        on=['fromdate','fromtime'],how='inner')

Entsoe_Final = pd.merge(Entsoe_Final1,Final_Day_ahead_Actual,
                        on=['fromdate','fromtime'],how='inner')

epex = pd.read_csv(r"C:\Users\mrudrappa\Desktop\Hackaton\hackathon\DayAheadAuctionEPEXSPOT.csv")


Datamart = pd.merge(epex,Entsoe_Final,
                        on=['fromdate','fromtime'],how='inner')

