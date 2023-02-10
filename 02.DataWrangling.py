
import pandas as pd

with open('data/bilancio_test.xml', 'r') as f:
    data = f.read()


# Questo codice da errore perché file probabilmente corrotto
df = pd.read_xml(data)


#ET.register_namespace("itcc-ci", "http://www.example.com/itcc-ci")
#df = pd.read_xml(data)


# Work-around per aprire il file e trasformarlo in pandas dataframe
import lxml.etree as ET

generator = ET.iterparse(r'data/bilancio_test.xml', events = ("start", "end"), recover = True, encoding = 'UTF-8')

data = []

for event, element in generator:
    if event == "end":
    # Raccogli i dati del tag e del testo
        tag = element.tag
        text = element.text
        if 'itcc-ci' in tag:
            data.append((tag, text))

df = pd.DataFrame.from_records(data, columns=["Tag", "Testo"])
df['Tag'] = df['Tag'].map(lambda x: x.lstrip('itcc-ci').replace(':',''))
df['Testo'] = df['Testo'].map(lambda x: x.replace('\n','').replace(' ',''))
df


df.to_csv('data/bilancio.csv', index = False, header = True)

