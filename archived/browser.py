from selenium import webdriver
from selenium.webdriver.firefox.firefox_binary import FirefoxBinary

binary = FirefoxBinary(r'C:\Program Files (x86)\Mozilla Firefox\firefox.exe')
browser = webdriver.Firefox(firefox_binary=binary)
browser.get('https://www.playgwent.com/en/redeem')

browser.find_element_by_id('beta_code_value').send_keys('SKJ52AA282C08E5E93')
# browser.find_element_by_class_name('recaptcha-checkbox-checkmark').click()
# browser.find_element_by_id('beta_code_save').click()

