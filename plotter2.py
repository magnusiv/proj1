# adal.f90 plotter. See adal.f90 for
# matrix sizes and name format. 

import numpy as np
import matplotlib.pyplot as plt

def exact(x):
    return 1.-(1.-np.exp(-10.))*x-np.exp(-10.*x)

n      = 13
errors = []
steps  = []

ax1 = plt.subplot(1,2,1)
ax1.set_title('Solution curves')
ax1.set_ylabel('$u(x)$')
ax1.set_xlabel(  '$x$' )
ax1.grid(True)

ax2 = plt.subplot(1,2,2)
ax2.set_title('Relative error')
ax2.set_ylabel('Sup$_i \; \epsilon_i $ ')
ax2.set_xlabel(  'log$_2 h$' )
ax2.grid(True)

for i in range(0,n):
    name = 'sol%2.0f.dat'%(i+11) 
    X = np.transpose(np.loadtxt(name))
    X = X[::-1]
    N = len(X); h = 1./N; x = np.linspace(h,1.-h,N)
    #________CURVES____________
    ax1.plot( x,X, label='N = %4.0f, log$_2 h$=%2.0f' % (N,np.log2(h)) )
    if i == n-1:
        EXACT = np.zeros(N)
        for l in range(0,N):
            EXACT[l] = exact(x[l])
        ax1.plot(x,EXACT,linewidth=2.,linestyle='--',label='Exact')
        print 'exact plotted'
    #_________Errors___________
    u        = np.zeros(N)
    for j in range(0,N):
        u[j] = np.log2(abs( (X[i]-exact(x[i]))/exact(x[i]) ))
    errors.append(  max(u)   )
    steps .append(np.log2(h))

ax1.legend(loc='center left', bbox_to_anchor=(1, 0.5))
ax2.plot(steps, errors, '-ro')
plt.show()
