cd /class/datamine/corporate/bayer/Data_Mine_Data/cluster_2 ##folder where the original data is
dir1=/class/datamine/corporate/bayer/Diana_Escamilla ##directory where you have your r script- resave.R
module load r 
for this_file in `ls ${1}`; do
	echo "trying ${this_file}"
	Rscript $dir1/resave.R ${this_file}
done
