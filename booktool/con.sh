for i in $@;do
iconv -f GBK -t UTF-8 $i -o $i;
done
