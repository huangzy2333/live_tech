import delimited "/Users/hwangsheep/Downloads/Info.csv", clear
import delimited "/Users/hwangsheep/Downloads/Enter.csv", clear
import delimited "/Users/hwangsheep/Downloads/tip_all-1110_2.csv", clear
import delimited "/Users/hwangsheep/Downloads/lowermean.csv", clear


summarize
asdoc sum frequency amount bvtextsim bvemosim vhonor livetype bgender numberviewers numberlikes wordcount, save datasum.doc replace stat(N mean sd min max) dec(3)

asdoc correlate frequency amount bvtextsim bvemosim vhonor livetype bgender numberviewers numberlikes wordcount, save datacorr.doc

asdoc pwcorr frequency amount bvtextsim bvemosim vhonor livetype bgender numberviewers numberlikes wordcount, sidak sig star(0.05) save datacorr.doc

gen lnfre = log(frequency)
gen lnamo = log(amount)
gen lnavgamo = log(amount/frequency)
gen lncon = log(bvtextsim)
gen lnemo = log(bvemosim)
gen lnlikes = log(numberlikes)
gen lnnum = log(numberviewers)
gen lnword = log(wordcount)
gen lndanmu = log(danmu_num)
gen text_emo = emo_sim*bvtextsim
gen emo_emo = emo_sim*bvemosim
gen text_honor = vhonor*bvtextsim
gen emo_honor = vhonor*bvemosim
gen text_type = livetype*bvtextsim
gen emo_type = livetype*bvemosim
gen text_time = timeslot*bvtextsim
gen emo_time = timeslot*bvemosim
gen lncon_honor = vhonor*lncon
gen lnemo_honor = vhonor*lnemo
gen lncon_type = livetype*lncon
gen lnemo_type = livetype*lnemo
//gen text_emo = bemosim*bvtextsim
//gen emo_emo = bemosim*bvemosim


//
pwcorr lnfre bvtextsim bvemosim  timeslot bgender livetype lnnum lnlikes lnword,sidak sig star(0.05)
correlate lnfre lnamo content emotion  tiphonor  bgender livetype lnnum lnlikes lnword


reg lnfre  bvtextsim  livetype timeslot vgender bgender timeslot lnword lnnum lnlikes lndanmu,robust
reg lnfre  bvtextsim bvemosim  livetype timeslot bgender  lnnum lnlikes lnword,robust
est store OLS

vif //多重共线性

hettest


//标准化
egen std_sum = std(tipping_sum)
egen mean_sum = mean(tipping_sum)
gen tipping_sum_norm = (tipping_sum - mean_sum) / std_sum

gen per_tip = amount/frequency

drop std_sum mean_sum

//面板数据

generate time_str = substr(filename, -2, 2)  //某列倒数第二位取两位数
destring time_str, generate(time)  //str转数字


//drop ls   //删掉某列
generate ls_str = substr(filename, 1, 7)
egen ls = group(ls_str)



xtset ls time
xtreg lnfre bvtextsim bvemosim vhonor vgender livetype bgender lnnum lnlikes lndanmu,fe

//GMM
tsset ls time

xtsum frequency bv_text_sim emo_sim honor_mean live_type

 xtabond2 frequency L.frequency bv_text_sim honor_mean gender_mean live_class, gmmstyle(L.frequency emo_sim honor_mean gender_mean live_class,l(1 1) collapse) ivstyle(bv_text_sim) twostep robust small orthogonal

 xtabond2 frequency L.frequency  bv_text_sim emo_sim honor_mean gender_mean live_type time_type, gmmstyle(L.frequency bv_text_sim emo_sim , collapse) ivstyle(honor_mean gender_mean live_type time_type) twostep robust 
 
 xtabond2 frequency L.frequency  bv_text_sim emo_sim honor_mean gender_mean live_type time_type , gmmstyle(frequency , l(1 1) collapse) gmmstyle(bv_text_sim emo_sim, collapse) ivstyle(L2.frequency honor_mean gender_mean live_type time_type) twostep robust  //结果很不错！不要动了
 
 xtabond2 frequency L.frequency  bv_text_sim emo_sim honor_mean gender_mean live_type time_type ls_gender lslikes, gmmstyle(frequency , l(1 1) collapse) gmmstyle(bv_text_sim emo_sim lslikes , collapse) ivstyle(L2.frequency honor_mean gender_mean live_type time_type ls_gender) twostep robust noleveleq
 
//主效应
reg lnfre  bvtextsim bvemosim  livetype vhonor vgender bgender timeslot lnnum lnlikes,robust
est store OLS

xtreg lnfre bvtextsim bvemosim vhonor vgender livetype bgender timeslot lnnum lnlikes,fe
est store FE

//text
xtabond2 lnfre L.lnfre  bvtextsim livetype vhonor bgender lnword lnnum lnlikes,  gmmstyle(lnfre, l(2 3) collapse) gmmstyle(bvtextsim L.bvtextsim, collapse) ivstyle(L2.lnfre livetype vhonor bgender lnnum L.lnnum lnlikes L.lnlikes) twostep robust or
//emo
xtabond2 lnfre L.lnfre bvemosim livetype vhonor bgender lnword  lnnum lnlikes,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvemosim L.bvemosim, collapse) ivstyle(L2.lnfre livetype vhonor bgender lnnum lnlikes ) twostep robust or
//text+emo
 xtabond2 lnfre L.lnfre  bvtextsim bvemosim livetype vhonor bgender lnword  lnnum lnlikes,  gmmstyle(lnfre, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre livetype vhonor bgender lnnum L.lnnum lnlikes L.lnlikes) twostep robust or
est store GMM3

esttab GMM1 GMM2 GMM3 using main_GMM.doc, scalar(N r2_a) star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)

esttab OLS FE GMM using xx.doc, scalar(N r2_a) star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)


//AVGAMOUNT
//主效应
xtabond2 lnavgamo L.lnavgamo  bvtextsim livetype vhonor bgender lnword  lnnum lnlikes,  gmmstyle(lnavgamo, l(2 3) collapse) gmmstyle(bvtextsim L.bvtextsim , collapse) ivstyle(L2.lnavgamo livetype vhonor bgender lnnum L.lnnum lnlikes L.lnlikes lnword) twostep robust or

xtabond2 lnavgamo L.lnavgamo  bvemosim livetype timeslot bgender lnword  lnnum lnlikes,  gmmstyle(lnavgamo, l(1 2) collapse) gmmstyle(bvemosim L.bvemosim , collapse) ivstyle(L2.lnavgamo livetype timeslot bgender lnnum L.lnnum lnlikes L.lnlikes lnword) twostep robust or


//text+emo
xtabond2 lnavgamo L.lnavgamo  bvtextsim bvemosim livetype vhonor bgender lnword  lnnum lnlikes,  gmmstyle(lnavgamo, l(2 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnavgamo livetype vhonor bgender lnnum L.lnnum lnlikes L.lnlikes lnword) twostep robust or

//调节效应
xtabond2 lnavgamo L.lnavgamo  bvtextsim bvemosim text_honor emo_honor livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnavgamo, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnavgamo vhonor livetype lnnum L.lnnum lnlikes L.lnlikes bgender lnword) twostep robust small or

xtabond2 lnavgamo L.lnavgamo  bvtextsim bvemosim text_type emo_type livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnavgamo, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnavgamo vhonor livetype lnnum L.lnnum lnlikes L.lnlikes bgender lnword) twostep robust small or 

xtabond2 lnavgamo L.lnavgamo  bvtextsim bvemosim text_honor emo_honor text_type emo_type livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnavgamo, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnavgamo vhonor livetype lnnum L.lnnum lnlikes L.lnlikes bgender lnword) twostep robust small or



//稳健性分析
*稳健性检验1：更换被解释变量方式（打赏次数，打赏金额）
xtabond2 lnamo L.lnamo  bvtextsim livetype timeslot bgender lnword  lnnum lnlikes,  gmmstyle(lnamo, l(1 2) collapse) gmmstyle(bvtextsim L.bvtextsim , collapse) ivstyle(L2.lnamo livetype timeslot bgender lnnum L.lnnum lnlikes L.lnlikes lnword ) twostep robust or

xtabond2 lnamo L.lnamo bvemosim livetype timeslot bgender lnword  lnnum lnlikes,  gmmstyle(lnamo, l(1 3) collapse) gmmstyle(bvemosim L.bvemosim, collapse) ivstyle(L2.lnamo livetype timeslot lnnum L.lnnum lnlikes L.lnlikes bgender lnword) twostep robust or

xtabond2 lnamo L.lnamo  bvtextsim bvemosim livetype timeslot bgender lnword  lnnum lnlikes,  gmmstyle(lnamo, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnamo livetype timeslot bgender lnnum L.lnnum lnlikes L.lnlikes lnword) twostep robust or
est store r1

//moderating
//
xtabond2 lnamo L.lnamo  bvtextsim bvemosim text_time emo_time livetype timeslot bgender lnnum lnlikes lnword,  gmmstyle(lnamo, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnamo timeslot livetype lnnum L.lnnum lnlikes L.lnlikes bgender lnword) twostep robust or
//
xtabond2 lnamo L.lnamo  bvtextsim bvemosim text_type emo_type livetype timeslot bgender lnnum lnlikes lnword,  gmmstyle(lnamo, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnamo timeslot livetype lnnum L.lnnum lnlikes L.lnlikes bgender lnword) twostep robust or

//
xtabond2 lnamo L.lnamo  bvtextsim bvemosim text_honor emo_honor text_type emo_type livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnamo, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnamo timeslot livetype lnnum L.lnnum lnlikes L.lnlikes bgender ) twostep robust or 
est store r2

esttab r1 r2 using xx.doc, scalar(N r2_a) star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)

*稳健性检验2：更换解释变量

*稳健性检验3：加入遗漏变量

*稳健性检验4：滞后一期被解释变量

//异质性分析
local m "xtabond2 lnfre L.lnfre  bvemosim emo_type livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvemosim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor livetype lnnum L.lnnum lnlikes bgender lnword) twostep robust small or" //自己的模型
bdiff, group(livetype) model(xtabond2 lnfre L.lnfre  bvemosim emo_type livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvemosim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor livetype lnnum L.lnnum lnlikes bgender lnword) twostep robust small or) reps(500) bs first detail //抽样500次

bdiff, group(livetype) model(xtreg lnfre bvtextsim bvemosim vhonor vgender livetype bgender lnlikes lnnum lnword,fe) reps(1000) bs first detail

bdiff, group(livetype) model(xtabond2 lnfre L.lnfre  bvtextsim bvemosim vhonor vgender livetype timeslot lnword , gmmstyle(lnfre) gmmstyle(bvtextsim bvemosim) ivstyle(L2.lnfre vhonor vgender livetype timeslot lnword) twostep robust) reps(100) bs first detail

reg lnfre  bvtextsim bvemosim vhonor bgender lnnum lnlikes lnword if livetype == 1
est store Info

reg lnfre  bvtextsim bvemosim vhonor  bgender lnnum lnlikes lnword if livetype == 0
est store Entertain

suest Info Entertain
test [Info_mean]bvemosim = [Entertain_mean]bvemosim

// 将结果保存到临时存储中
estimates store suest_results

// 导出结果到外部文件
esttab suest_results using "suest_results.doc", replace

//调节效应

//honor
 xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_honor emo_honor livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor livetype lnnum L.lnnum lnlikes bgender lnword) twostep robust small or

//type
xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_type emo_type livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor livetype lnnum L.lnnum lnlikes bgender lnword) twostep robust small or

//honor+type
 //xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_honor emo_honor text_type emo_type livetype vhonor vgender bgender timeslot lnnum lnlikes,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor livetype lnnum L.lnnum) twostep robust small or

 xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_honor emo_honor text_type emo_type livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor livetype lnnum L.lnnum lnlikes bgender lnword) twostep robust small or 


//倒U
gen bvtextsim2 = bvtextsim*bvtextsim
gen bvemosim2 = bvemosim*bvemosim
xtset ls time
xtreg lnfre bvtextsim bvtextsim2 bvemosim vhonor bgender lnnum lnlikes lnword ,fe
xtreg lnfre bvtextsim bvemosim2 bvemosim vhonor bgender lnnum lnlikes lnword,fe
reg lnfre bvtextsim bvemosim bvemosim2 vhonor bgender lnnum lnlikes lnword livetype,robust
utest bvtextsim bvtextsim2 , fieller level(99)
utest bvemosim bvemosim2 , fieller level(99)

sum bvemosim

//单直播类型
//主效应
//text
xtabond2 lnfre L.lnfre  bvtextsim vhonor bgender lnword lnnum lnlikes,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim L.bvtextsim, collapse) ivstyle(L2.lnfre vhonor bgender lnnum lnlikes) twostep robust or
est store info1

//emo
xtabond2 lnfre L.lnfre bvemosim vhonor bgender lnword  lnnum lnlikes,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvemosim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor bgender lnnum lnlikes) twostep robust or
est store info2

//text+emo
 xtabond2 lnfre L.lnfre  bvtextsim bvemosim vhonor bgender lnword  lnnum lnlikes,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor bgender lnnum lnlikes) twostep robust or
est store info3
 
 //调节效应
 //honor
xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_honor emo_honor vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre vhonor  lnnum L.lnnum lnlikes bgender lnword) twostep robust small or

est store info4


//emo_sim
xtabond2 lnfre L.lnfre  bvtextsim bvemosim emo_sim livetype vhonor bgender  lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre livetype vhonor bgender) twostep robust or

 xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_emo emo_emo text_type emo_type emo_sim livetype vhonor bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre emo_sim vhonor livetype lnnum L.lnnum lnlikes bgender lnword) twostep robust small or 
 
 
//time slot
//text
xtabond2 lnfre L.lnfre  bvtextsim livetype timeslot bgender lnword lnnum lnlikes,  gmmstyle(lnfre, l(1 2) collapse) gmmstyle(bvtextsim L.bvtextsim, collapse) ivstyle(L2.lnfre livetype timeslot bgender lnnum L.lnnum lnlikes L.lnlikes) twostep robust or

//emo
xtabond2 lnfre L.lnfre  bvemosim livetype timeslot bgender lnword lnnum lnlikes,  gmmstyle(lnfre, l(2 3) collapse) gmmstyle(bvemosim L.bvemosim, collapse) ivstyle(L2.lnfre livetype timeslot lnnum L.lnnum lnlikes bgender lnword) twostep robust or

//text+emo
 xtabond2 lnfre L.lnfre  bvtextsim bvemosim livetype timeslot bgender lnnum lnlikes lnword, gmmstyle(lnfre, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre timeslot livetype lnnum lnlikes bgender lnword) twostep robust small or

 //moderating timeslot
 xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_time emo_time livetype timeslot bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre timeslot livetype lnnum L.lnnum lnlikes bgender lnword) twostep robust small or
 
xtabond2 lnfre L.lnfre  bvtextsim bvemosim text_type emo_type livetype timeslot bgender lnnum lnlikes lnword,  gmmstyle(lnfre, l(1 3) collapse) gmmstyle(bvtextsim bvemosim L.bvtextsim L.bvemosim, collapse) ivstyle(L2.lnfre  livetype timeslot lnnum lnlikes lnword bgender) twostep robust small or
