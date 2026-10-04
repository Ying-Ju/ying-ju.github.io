from pathlib import Path
import json,numpy as np,pandas as pd
from scipy import stats,optimize,special
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
ROOT=Path(__file__).parent
(ROOT/'figures-python').mkdir(exist_ok=True)
plt.rcParams.update({'font.size':13,'axes.spines.top':False,'axes.spines.right':False,'axes.titleweight':'bold','figure.facecolor':'#faf9f5','axes.facecolor':'#faf9f5','savefig.facecolor':'#faf9f5'})
colors={'Normal':'#287c8e','t':'#d97938','Weibull':'#7963a6','Gamma':'#287c8e','Lognormal':'#d97938'}
base=pd.read_csv(ROOT/'data/fastballs.csv')
base[['player_name','pitcher','game_date','game_pk','release_speed','pitch_type','game_type']].to_csv(ROOT/'data/fastballs.csv',index=False)
rows=[];fig,axs=plt.subplots(1,3,figsize=(14,4.5),sharex=True,sharey=True)
models={}
for ax,(name,d) in zip(axs,base.groupby('player_name')):
 v=d.release_speed.dropna().values;models[name]={};ax.hist(v,bins=np.arange(89,100.25,.25),density=True,color='#d6e6e9',edgecolor='white');grid=np.linspace(89,100,900)
 for label,dist,k,kw in [('Normal',stats.norm,2,{}),('t',stats.t,3,{}),('Weibull',stats.weibull_min,2,{'floc':0})]:
  p=dist.fit(v,**kw)
  if label=='t':
   def nllt(theta):
    return -np.sum(stats.t.logpdf((v-theta[0])/np.exp(theta[1]),np.exp(theta[2]))-theta[1])
   opts=[optimize.minimize(nllt,[v.mean(),np.log(v.std()),np.log(df)],method='Nelder-Mead',options={'maxiter':10000,'xatol':1e-8,'fatol':1e-8}) for df in [10,100,1000]]
   opt=min(opts,key=lambda o:o.fun)
   p=(np.exp(opt.x[2]),opt.x[0],np.exp(opt.x[1]))
  aic=2*k-2*dist.logpdf(v,*p).sum();models[name][label]=(dist,p);rows.append({'player':name,'model':label,'n':len(v),'AIC':aic,'parameters':list(map(float,p))});ax.plot(grid,dist.pdf(grid,*p),color=colors[label],lw=2,label=label)
 ax.set_title(name.split(',')[1].strip()+' '+name.split(',')[0]);ax.set_xlabel('Four-seam speed (mph)');ax.set_xlim(89,100);ax.grid(alpha=.12)
axs[0].set_ylabel('Density');axs[2].legend(frameon=False);fig.tight_layout();fig.savefig(ROOT/'figures-python/baseball_fits.png',dpi=170);plt.close(fig)
pd.DataFrame(rows).to_csv(ROOT/'data/baseball_fit_results.csv',index=False)
fig,axs=plt.subplots(1,3,figsize=(14,4.5))
for ax,(name,d) in zip(axs,base.groupby('player_name')):
 v=np.sort(d.release_speed.dropna());probs=(np.arange(len(v))+.5)/len(v)
 for label,(dist,p) in models[name].items():ax.scatter(dist.ppf(probs,*p),v,s=7,alpha=.5,color=colors[label],label=label)
 ax.plot([88,101],[88,101],color='gray',ls='--');ax.set(xlim=(88,101),ylim=(88,101),xlabel='Model quantiles (mph)',title=name.split(',')[0]);ax.set_aspect('equal');ax.grid(alpha=.12)
axs[0].set_ylabel('Observed quantiles (mph)');axs[2].legend(frameon=False,fontsize=10);fig.tight_layout();fig.savefig(ROOT/'figures-python/baseball_qq.png',dpi=170);plt.close(fig)
f=pd.read_csv(ROOT/'data/police_repairs_2022.csv');f.to_csv(ROOT/'data/police_repairs_2022.csv',index=False);v=f.labor_hours.values;v=v[v>0];repair=[];fig,ax=plt.subplots(figsize=(11,5));ax.hist(v,bins=np.arange(0,89,.5),density=True,color='#d6e6e9',edgecolor='white');grid=np.linspace(.001,88,3000)
figq,axq=plt.subplots(figsize=(6,6))
for label,dist in [('Gamma',stats.gamma),('Lognormal',stats.lognorm),('Weibull',stats.weibull_min)]:
 p=dist.fit(v,floc=0);aic=4-2*dist.logpdf(v,*p).sum();repair.append({'model':label,'AIC':aic,'over8':dist.sf(8,*p),'mean':dist.mean(*p)});ax.plot(grid,dist.pdf(grid,*p),color=colors[label],lw=2,label=label);prob=(np.arange(len(v))+.5)/len(v);axq.scatter(dist.ppf(prob,*p),np.sort(v),s=8,alpha=.5,color=colors[label],label=label)
ax.set(xlim=(0,20),xlabel='Positive recorded labor hours',ylabel='Density');ax.legend(frameon=False);ax.grid(alpha=.12);fig.tight_layout();fig.savefig(ROOT/'figures-python/repair_fits.png',dpi=170);plt.close(fig)
axq.plot([0,90],[0,90],ls='--',color='gray');axq.set(xlim=(0,90),ylim=(0,90),xlabel='Model quantiles (labor hours)',ylabel='Observed quantiles (labor hours)');axq.set_aspect('equal');axq.legend(frameon=False);figq.tight_layout();figq.savefig(ROOT/'figures-python/repair_qq.png',dpi=170);plt.close(figq);pd.DataFrame(repair).to_csv(ROOT/'data/repair_fit_results.csv',index=False)
d=pd.read_csv(ROOT/'data/helene_claims.csv')
y=pd.to_numeric(d.netBuildingPaymentAmount).values;cap=250000.;inside=y[(y>0)&(y<cap)];insurance=[];fig,ax=plt.subplots(figsize=(11,5));ax.hist(y,bins=np.arange(0,260000,10000),color='#287c8e',edgecolor='white');ax.set(xlabel='Net building payment (USD)',ylabel='Number of claims');ax.ticklabel_format(axis='x',style='plain');ax.text(.98,.95,f'{sum(y==0)} zero payments\n{sum(y==cap)} payments at $250,000',transform=ax.transAxes,ha='right',va='top');fig.tight_layout();fig.savefig(ROOT/'figures-python/insurance_hist.png',dpi=170);plt.close(fig)
# Conditional interior model: independent point masses at 0 and cap,
# plus a continuous distribution conditioned on 0<X<cap. No latent-loss inference.
fig,ax=plt.subplots(figsize=(11,5));z=inside/cap;grid=np.linspace(.0001,.9999,1500)
ax.hist(inside,bins=np.arange(0,260000,10000),density=True,color='#d6e6e9',edgecolor='white')
for label,dist in [('Gamma',stats.gamma),('Lognormal',stats.lognorm)]:
 start=dist.fit(z,floc=0);theta=np.log([start[0],start[2]])
 def nll(t):
  shape,scale=np.exp(t);logF=dist.logcdf(1,shape,loc=0,scale=scale)
  return -(dist.logpdf(z,shape,loc=0,scale=scale).sum()-len(z)*logF)
 opt=optimize.minimize(nll,theta,method='Nelder-Mead',options={'maxiter':3000,'xatol':1e-9});a,s=np.exp(opt.x);F=dist.cdf(1,a,scale=s);ax.plot(grid*cap,dist.pdf(grid,a,scale=s)/F/cap,lw=2,color=colors[label],label=label+' (truncated)');insurance.append({'model':label,'AIC':4+2*opt.fun,'shape':a,'scale_fraction_of_cap':s,'optimizer_success':bool(opt.success)})
ax.set(xlabel='Interior payment (USD)',ylabel='Conditional density');ax.legend(frameon=False);fig.tight_layout();fig.savefig(ROOT/'figures-python/insurance_body.png',dpi=170);plt.close(fig)
pd.DataFrame(insurance).to_csv(ROOT/'data/insurance_fit_results.csv',index=False)
print('REPAIR',repair);print('INSURANCE',len(y),len(inside),insurance)
