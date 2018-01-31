
「R 軟體：應用統計方法」光碟內容

光碟裡面包含以下項目：

1. R-2.11.1-win32.exe     一般 Windows 作業系統安裝檔(32 位元 Windows 版本)
   R-2.11.1-win64.exe     64 位元 Windows 版本安裝檔
   R-2.11.1.tar.gz        UNIX 作業系統自行安裝原始碼

2. Data 目錄：包含 rda, txt, csv 三種格式的檔案

3. R_Examples 目錄：包含各章節內比較完整的範例檔案

4. R_Plots 目錄：包含本書幾個複雜圖形的原始檔

5. R_Functions 目錄：本書作者所寫的一些 R 自訂函數


使用資料檔：

   請將資料檔 copy 到您的 R 軟體的工作目錄下，並用
   read.table 輸入 .txt 資料檔，用 read.csv 輸入 .csv 資料檔，
   或用 load("rda 檔路徑") 載入 .rda 資料檔
   
使用程式檔：   

   請將程式檔 copy 到您的 R 軟體的工作目錄下，
   並在 R 軟體中使用 
   
         source("程式檔路徑") 
   
   載入執行，或直接於 R 軟體「檔案」選項底下「開啟命令稿」載入執行
   
      