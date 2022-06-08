from porterStemmer2 import *

# Each CNBC news have a company name in parenthesis
def check_for_company_in_line(S):
    name_list = []
    for word in S:
        if (word[0] == '(' and word != S[-1] and word[-1] == ')'):
            word = word.replace('(', '')
            word = word.replace(')', '')
            name_list += [word]
    return name_list

positiveWords = ["increas"]
negativeWords = ["decreas"]

def sentiment_value(line):
    #print line
    for word in line:
        #print word
        if (stem(word) == 'increas'):
            print "Go here"
        if (stem(word.lower()) in positiveWords):
            print "Go here +1"
            return 1
        elif (stem(word.lower()) in negativeWords):
            print "Go here -1"
            return -1
    return 0



vocabulary = set();
f = open('stockDocThreeMonth.txt', 'r')

#New news (document) appears after these marker.
markerList = ["***March","***April","***May","***June"]

""" These next steps will count the number of time words in the vocabulary
appears in each document and put them in the matrix which will be used later
in the analysis of distance or nonnegative matrix factorization  
"""
Date = ""
sentiment_company = {}
print Date
for line in f:
    Line = line.split()
    if (len(Line) > 0 and Line[0] in markerList):
        Month = Line[0].replace('*','')
        Day = Line[1].replace('*','')
        Date = Month + ' ' + Day
        #print Date
    #check if line mentions any company
    # if not, just skip the line
    company_name_list = check_for_company_in_line(Line)
    if (len(company_name_list) != 0):            
        #print company_name_list
        ##############################
        sentiment = sentiment_value(Line) # Hardest part of this project, so I made a separate function
        ##############################
        if (sentiment != 0):
            print line
        #Update the sentiment value for each company on each date
        for name in company_name_list:
            sentiment_company[(Date, name)] = sentiment
            
#print sentiment_company
print len(sentiment_company)
