import pyreadstat
import pandas as pd
#df=pd.read_sas('bw.xpt',encoding='utf-8')
df, meta= pyreadstat.read_xport('bw.xpt')

#print(df.head())
#print(meta.column_labels)
#print(dir(df))
print(df.info)
