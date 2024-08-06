import codecs
from openpyxl import Workbook

def cal_rrss(data): # 输入参数data是一个包含实验数据的列表，每个元素代表一次试验的数据，以制表符（\t）分隔的字符串
	# 函数内部通过遍历data，区分"切换试次"和"重复试次"，分别计算它们的反应时间和正确性，以及调整后的反应时间（只计算正确响应的）
	# 最后，计算并返回平均正确率、调整后的切换成本、调整后的切换和重复试次的平均反应时间
	index = [-8, -2] # 指定在分割的数据中哪些列是重要的，-8 和 -2 分别可能代表正确性和反应时间
	switch_ACC = 0
	RT_duplicate = 0
	RT_switch = 0
	adjRT_duplicate = 0
	adjRT_switch = 0
	num_duplicate = 0
	num_switch = 0

	yellow_index = [-29,-28,-26,-24,-21,-20,-19,-17,-15,-13,-10,-8,-7,-6,-4,-1] # 包含了判定为任务切换试次的索引值
	for i in range(-32, 0):
		d = data[i].split('\t')
		if i in yellow_index:
			RT_switch += int(d[index[1]])
			switch_ACC += int(d[index[0]])
			if d[index[0]] == '1':
				adjRT_switch += int(d[index[1]])
				num_switch += 1
		else:
			RT_duplicate += int(d[index[1]])
			if d[index[0]] == '1':
				adjRT_duplicate += int(d[index[1]])
				num_duplicate += 1

	RT_duplicate /= 16.0
	RT_switch /= 16.0
	switch_ACC /= 16.0
	adjRT_duplicate /= float(num_duplicate)
	adjRT_switch /= float(num_switch)
	adj_switch_cost = adjRT_switch - adjRT_duplicate

	return str(switch_ACC) + ' ' + str(adj_switch_cost) + ' ' + str(adjRT_switch) + ' ' + str(adjRT_duplicate) + '\n'


def saveRes(sub, ses, data):
    formatted_sub = sub.zfill(4)
    formatted_ses = ses.zfill(2)
    calculation_result = cal_rrss(data)
    # 直接返回包含所有数据的列表
    return [formatted_sub, formatted_ses] + calculation_result.split()

def process_file(filename, ses, results):
    with codecs.open(filename, 'r', 'utf-16') as f:
        lines = f.readlines()

    sub = ''
    data = []

    for l in lines[2:]:
        ll = l.split('\t')
        if not ll or len(ll) < 2:
            continue

        current_sub = ll[1].strip()
        if current_sub != sub:
            if sub:
                results.append(saveRes(sub, ses, data))
            sub = current_sub
            data = [l]
        else:
            data.append(l)

    if data:
        results.append(saveRes(sub, ses, data))

results = []

for i, file in enumerate(['wave1.txt', 'wave2.txt', 'wave3.txt'], start=1):
    ses = str(i)
    process_file(file, ses, results)

# 创建一个新的Excel工作簿
wb = Workbook()
ws = wb.active

# 添加标题行
ws.append(['sub', 'ses', 'ACC', 'switchCost', 'switchRT', 'repeatRT'])

# 添加数据行
for result in results:
    ws.append(result)

# 保存工作簿到文件
wb.save("TaskSwitch_Arr.xlsx")