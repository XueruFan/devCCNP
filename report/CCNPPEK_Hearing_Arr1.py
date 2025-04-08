# 本代码用来规范化处理彩巢听觉数据原始文件夹的名字
# 范雪如 24年大年初四于北师大
import os
import re

def rename_folders(directory):
    # 遍历指定目录下的所有项
    for filename in os.listdir(directory):
        # 确定当前项是一个文件夹
        if os.path.isdir(os.path.join(directory, filename)):
            # 移除文件夹名中的汉字
            new_name = re.sub('[\u4e00-\u9fa5]', '', filename)
            # 检查处理后的文件夹名是否包含w
            if 'w' not in new_name.lower():
                # 如果不包含w，则在末尾添加_w1
                new_name += '_w1'
            # 构建旧路径和新路径
            old_path = os.path.join(directory, filename)
            new_path = os.path.join(directory, new_name)
            # 重命名文件夹
            os.rename(old_path, new_path)
            print(f"Renamed '{old_path}' to '{new_path}'")

# 指定需要处理的目录路径
directory = "//172.16.191.42/home/项目管理/CCNP/数据电子化/自处理数据/听觉原始文件/Batch4/原始文件"
rename_folders(directory)

