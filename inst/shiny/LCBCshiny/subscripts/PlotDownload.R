PlotDownload = function(df,type){

  fheight = 10
  fwidth = 10
  fres = 300
  
  if(type=="pdf"){
    fheight = round(fheight*0.3937,2)
    fwidth = round(fwidth*0.3937,2)
  }
  
  switch(type,
    "png"  = png(filename, height=fheight, width=fwidth, res=fres, units="cm"),
    "tiff" = tiff(filename, height=fheight, width=fwidth, res=fres, units="cm",compression="lzw"),
    "jpeg" = jpeg(filename, height=fheight, width=fwidth, res=fres, units="cm",quality=100),
    "pdf"  = pdf(filename, height=fheight, width=fwidth))
  plot(df)
  dev.off()
}