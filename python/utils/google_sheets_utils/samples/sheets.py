# Full getting started tutorial by Greg Baugues at:
# https://www.twilio.com/blog/2017/02/an-easy-way-to-read-and-write-to-a-google-spreadsheet-in-python.html

import gspread
from oauth2client.service_account import ServiceAccountCredentials
 
# use creds to create a client to interact with the Google Drive API
scope = ["https://spreadsheets.google.com/feeds"]
creds = ServiceAccountCredentials.from_json_keyfile_name("client_secret.json", scope) # json file from your Google API Console
client = gspread.authorize(creds)
 
# Find a workbook by name and open the first sheet
sheet = client.open("name_of_your_google_sheet").sheet1
 
# Extract and print all of the values
list_of_hashes = sheet.get_all_records()
print(list_of_hashes)

# get a list of lists if you'd prefer
sheet.get_all_values()

# pull data from a single row, column, or cell
sheet.row_values(1)
sheet.col_values(1)
sheet.cell(1, 1).value

# write to spreadsheet by changing a specific cell
sheet.update_cell(1, 1, "I'm writing to a spreadsheet using Python!")

# insert a row in the spreadsheet
row = ["I'm","inserting","a","row","into","a,","spreadsheet","with","Python"]
index = 1
sheet.insert_row(row, index)

# delete a row from a spreadsheet
sheet.delete_row(1)

# find total number of rows
sheet.row_count