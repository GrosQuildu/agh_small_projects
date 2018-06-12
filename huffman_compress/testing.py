#!/usr/bin/python

import string, random, subprocess, time, hashlib, sys, datetime, os

def test():
    if subprocess.check_call(['./pp_compress','test.txt'])==0:
        if subprocess.check_call(['./pp_compress','-d','test.txt.pp'])==0:
            with open('test.txt','r') as f:
                h1=hashlib.md5(f.read()).digest()
            with open('test.txt.pp.dc','r') as f:
                h2=hashlib.md5(f.read()).digest()
            if h1!=h2:
                print "Nie sa identyczne"
                return 1
        else:
            print "Blad dekompresji"
            return 1
    else:
        print "Blad kompresji"
        return 1
    return 0

def sprawdz(dlugosc, tekst=False, ile_prob=10):
    print "Dlugosc",dlugosc,
    czas=0
    for prob in xrange(ile_prob):
        if dlugosc<1024*1024 or tekst==True: #<1MB
            f=open('test.txt','wb')
            for x in xrange(dlugosc):
                if tekst:
                    tmp=string.printable[random.randint(0,99)]
                else:
                    tmp=chr(random.randint(0,255))
                f.write(tmp)
            f.close()
        else:
            subprocess.call(['dd', 'if=/dev/urandom', 'of=test.txt', 'bs=1048576', 'count='+str(dlugosc/1048576)], stdout=open(os.devnull, "w"), stderr=open(os.devnull, "w"))
        start=time.time()
        if test()==1:
            sys.exit(1)
        czas+=time.time()-start
    print "zaliczona, sredni czas: "+str(datetime.timedelta(seconds=czas/ile_prob))+", kompresja: "+str("{0:.2f}".format(os.path.getsize('test.txt')/float(os.path.getsize('test.txt.pp'))))+"%"

print "[+]Testy male"
for dlugosc in xrange(2,1000,7):
    sprawdz(dlugosc)

print "[+]Testy wieksze"
for count in xrange(20):
    dlugosc=random.randint(10000,100000)
    sprawdz(dlugosc)

print "[+]10MB losowe"
sprawdz(1024*1024*10, False, 2)

print "[+]1GB losowe"
sprawdz(1024*1024*1024, False, 1)

print "[+]Tekst 2GB"
sprawdz(1024*1024*1024*2, True, 1)