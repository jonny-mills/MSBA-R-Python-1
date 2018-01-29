# -*- coding: utf-8 -*-
"""
Created on Thu Sep 28 13:48:26 2017
@author: Jonny Mills
"""

#User enters in 2 parameters, (N & k), and the computer does a bisection search for the kth root of N
# it stops looking when it finds a number within .001 of the target (the Kth root of N)
#This program is am improvement upon bubble sort, which has a complexity of O(n log n)
#The time complexity of the binary search algorithm belongs to the O(log n) class

def bisection_search_kth_root(N,k):
    epsilon = 0.001 
    low = 0.0
    high = 100000.0
    target = N - epsilon 
    idx = False
    
    
    while high >= low and not idx:
        mid = low + (high - low)/2.0
        x = mid
        print("x", x)
        print("high", high)
        print("low", low)
        if abs(x ** k - N) <= abs(epsilon): #if (x ** k - N) is less than epsilon, our margin of error, then the problem is solved!
            return (x)
        elif x ** k > target: #otherwise, if this condition is met,  the high becomes the midpoint, and then is lowered by an infintesimle amount
            high = mid - epsilon/(100 ** 100)
        else:
            low = mid + epsilon/(100 ** 100) #and vise versa
    return False

N = 25.0
k = 2.0
print (bisection_search_kth_root(N,k))

