import os
import random
commands = ['mkdir','touch','mv','cp','rm','test']
arg_no = [1, 1, 2, 2, 1, 1]
is_file = [0, 1, 2, 2, 2, 2]
options = [['', ''], ['', ''], ['', ''], ['', '-r '], ['', '-r '], ['-f ', '-d ', '-e ']]
no_of_cmd = 15

root = "/bin"
dirs = []

def add_dir(link):
    if(len(dirs)>1000):
        return
    curr = os.listdir(link)
    for i in curr:
        try:
            if i not in dirs and len(i)<15:
                dirs.append(i)
            add_dir(link+"/"+i)
        except:
            pass

add_dir(root)


def gen_link():
    link = ""
    rel = ['..', '.']
    link_len = 1+int(random.random()*10)
    for i in range(link_len):
        curr = None
        if random.random()<0.1:
            curr = rel
        else:
            curr = dirs
        link += curr[int(len(curr) * random.random()) - 1]
        if not i == link_len - 1:
            link += "/"
    return link

comms = ""
for i in range(no_of_cmd):
    inx = int(random.random()*len(commands))
    comms += commands[inx]
    will_be_file = False
    if is_file[inx]==2:
        will_be_file = random.random()<0.5
    elif is_file[inx] == 1:
        will_be_file = True
    link = gen_link()
    if not will_be_file:
        link += "/" 
    if not commands[inx] == 'test':
        comms += " " + options[inx][not will_be_file] + link
    else:
        comms += " " + (options[inx][not will_be_file] if random.random()>0.5 else options[inx][2]) + link
    
    if will_be_file:
        while comms.endswith(".."):
            comms = comms[:-4]
    if arg_no[inx] == 2:
        comms += " "+gen_link()+"/"
    comms += "\n"

file_info = open("cmd.dat", "w+")
file_info.write(comms)
file_info.flush() 
