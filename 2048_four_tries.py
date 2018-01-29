#I wrote this program to allow the computer to automatically play 2048.
#If your computer doesn't selenium or webdriver to run the code, it may not run.
# see video tutorial here to watch the program in action: https://www.screencast.com/t/oleCu3Oy

from selenium import webdriver 
from selenium.webdriver.common.keys import Keys 


browser = webdriver.Firefox() 
browser.get('https://gabrielecirulli.github.io/2048/') #url
htmlElem = browser.find_element_by_tag_name('html') 

for i in range(5):
    for i in range(256): 
        htmlElem.send_keys(Keys.DOWN)
        htmlElem.send_keys(Keys.LEFT)
        htmlElem.send_keys(Keys.UP)
        htmlElem.send_keys(Keys.RIGHT)
    linkElem = browser.find_element_by_link_text('Try again') 
    linkElem.click()






