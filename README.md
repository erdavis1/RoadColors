# RoadColors

United States:
Plot roads in a US city, coloring each by its designation (e.g. Street, Road, Avenue)

Before you get started, you’ll need to get a few things prepared:

1. Find the lat/long point you’d like to be at the center of your map
2. Identify the GEOIDs of counties that are within 15 miles of this point. This site is very helpful: https://census.missouri.edu/geocodes/
3. Download roads shapefiles for the counties in step 2: 
ftp://ftp2.census.gov/geo/tiger/TIGER2018/ROADS/
4. Download feature names shapefiles for the counties in step 2: https://www2.census.gov/geo/tiger/TIGER2018/FEATNAMES/

This code isn't as well commented as Canada or Worldwide, so you might want to start with either of those first.

Canada:
Plot roads in a Canadian city, coloring each by its designation

Worldwide:
Plot roads in any worldwide city using OpenStreetMap data. It derives road designations from the road name. I only wrote this to work for countries that use English or Romance languages--it doesn't work for languages like German (which mashes the road designation into the road name with no spaces) or languages that don't use the Latin alphabet.
