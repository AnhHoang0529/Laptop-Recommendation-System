import sklearn
import streamlit as st
st.set_page_config(page_title = "Laptop Recommendation Systems", layout="wide")

# @st.cache(suppress_st_warning = True)

import pandas as pd
# load the dataframe and features
df = pd.read_csv("./demo/tidy_data.csv")
features = pd.read_csv("./demo/features")

from sklearn.neighbors import NearestNeighbors
from sklearn.preprocessing import MinMaxScaler
def kNNeighbors(f, n):

    minmax = MinMaxScaler()
    features = minmax.fit_transform(f)
    model = NearestNeighbors(n_neighbors = n, algorithm = 'brute', metric = 'cosine')
    model.fit(features)
    dist, id = model.kneighbors(features)

    return dist, id

def Recommand(df,name,id):
    lap_list = []
    lap_id = df[df['Name'] == name].index
    lap_id = lap_id[0]
    for new in id[lap_id]:
        lap_list.append(df.loc[new])
    return lap_list

title = 'Laptop Recommendation System'
st.title(title)
#Now we will take user input one by one as per our dataframe

name_lap = st.text_input('Name laptop: ')

#Prediction
if st.button('Search'):

    dist, id = kNNeighbors(features, 10)
    recommender = Recommand(df,name_lap,id)
    st.header("Recommended laptops: ")
    st.dataframe(pd.DataFrame(recommender).iloc[: , 1:])