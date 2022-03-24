from bs4 import BeautifulSoup as bs

files = ["e-commerce_q2_2021","e-commerce_q3_2021","e-commerce_q4_2021"]

for filename in files:
    with open("RawData/"+filename+".html",'r') as f:
        doc = bs(f, "html.parser")

    headerContents = doc.find("div", class_="header")
    headerContents = headerContents.find_all("span")

    header = []

    for item in headerContents:
        header.append(item.string.strip(" \n"))


    # toko              <label>
    # web               <span><p>
    # rankingAppStore   <span>
    # rankingPlayStore  <span>
    # twitter           <span><p>
    # instagram         <span><p>
    # facebook          <span><p>
    # karyawan          <span><p>

    rowData = []

    for item in doc.find_all("div", class_="row"):
        row = []
        toko = item.find("label")
        row.append(toko.string.strip(" \n"))

        spans = item.find_all("span")[1:] # skip the first span which contains label tag for nama toko

        for span in spans:
            contents = list(span.descendants)
            if len(contents)>1:
                row.append(contents[2].strip(" \n"))
            else:
                row.append(contents[0].strip(" \n"))

        rowData.append(row)

    import pandas as pd
    import numpy as np

    header = np.array(header)
    rowData = np.array(rowData)

    df = pd.DataFrame(columns=header)

    for i in range(len(rowData)):
        df.loc[i] = rowData[i]


    df.to_csv("Datasets/"+filename+".csv", index=False)