# Laptop Recommendation System
This is the repository for Final Project of DS103 (Data Collecting and Processing) of University of Information Technology (UIT). In this project, we crawl laptop data from local e-commerce websites and build a simple recommendation system according to the configuration.

<h3> Laptop Dataset </h3>

Data collected from 5 e-commerce websites:
- CellphoneS: https://cellphones.com.vn/laptop.html
- Sendo: https://www.sendo.vn/laptop
- Thegioididong: https://www.thegioididong.com/laptop
- Tiki: https://tiki.vn/laptop/c8095
- FPTshop: https://fptshop.com.vn/may-tinh-xach-tay

The dataset is collected and processed by Anh Thi-Hoang Nguyen, Dung Ha Nguyen, Nghia Hieu Nguyen và Qua Thanh-Thien Nguyen, Faculty of Information Science and Technology, University of Information Technology, Vietnam National University, Ho Chi Minh City.

If you have any questions, get in touch by email 20520134@gm.uit.edu.vn or 20520165@gm.uit.edu.vn

The data set is collected for the purpose of building a laptop recommendation system based on computer configuration.

The tuple consists of 2086 rows and 11 attributes:
- ID (integer): Laptop id code in dataframe
- Brand (char): Brand of laptop
- Name (char): Name of laptop
- ScreenSize (float): Screen size (unit: inch)
- CPU (char): CPU name of laptopp
- RAM (integer): Ram size of laptop (unit: GB)
- DefaultPrice (integer): Original price of laptop
- SalePrice (integer): Promotional price of laptop
- HDD (integer): Size of laptop's HDD (unit: GB)
- SSD (integer): Laptop SSD size (unit: GB)
- eMMC (integer): Size of laptop eMMC drive (unit: GB)
- Link (char): Link to buy laptop
- Website (char): Website selling laptop

# Demo in Command Prompt

<h3>Set up the enviroment </h3>

```
git clone https://github.com/AnhHoang0529/Laptop-Recommendation-System.git

cd Laptop-Recommendation-System
```

<h3>Create Anaconda Virtual Environment And Install Packages </h3>

```
conda create -n demo python=3.6

conda activate demo

pip install -r requirements.txt
```

<h3>Demo using Streamlit </h3>

```
streamlit run app.py
```
