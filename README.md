## ProjetEZ

### Agrege folder


Agregated data are in the folder called "Agrege/Base". Here you can find subfolder from the several databases used (ECB, EUROSTAT, IMF, AWM, Datastream). The original database are in .csv in these subfolders. 

The .R file called "transformation" corresponds to the code used to modify the variables (log ..). 

The .pdf file called "database_description" gives an overview of the database and the transformations made. 

If you want to use the dataset of agregated data for the EZ that we used in our estimation, load the database called "vardata.Rdata" in the folder /Agrege/Base.  


### data folder

In the data folder you will find the two main database. The first one is monthly frequency, the other is quarterly frequency. The .pdf file in the subfolder /data and /data/Quarterly describe how we transform the database. 

Raw data loaded from the several databases used are in the subfolders names from these databases (ex : /ECB ). In each subfolder, a .R file called "traitement..." load raw dataset and transform them. Then We save these transformed data into .RData file. 

Finally, we merged these data and created 4 main files :
1. vardatam.RData : monthly database transformed 
2. difvardatam.RData : monthly database transformed and differentiated once
3. vardataq.RData : quarterly database transformed 
4. difvardataq.RData : quarterly database transformed and differentiated once
