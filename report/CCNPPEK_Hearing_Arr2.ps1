# 指定脚本编码为 UTF-8 with BOM
[System.Text.Encoding]::UTF8

# 设置源目录路径（使用UNC路径）
$sourceDir = "\\172.16.191.42\home\项目管理\CCNP\数据电子化\自处理数据\听觉原始文件\Batch4\原始文件"

# 检查源目录是否存在
if (-not (Test-Path -Path $sourceDir)) {
    Write-Host "源目录不存在或无法访问: $sourceDir" -ForegroundColor Red
    exit
}

# 遍历顶层目录（如004_w2）
Get-ChildItem -Path $sourceDir -Directory | ForEach-Object {
    $projectDir = $_.FullName
    Write-Host "正在处理目录: $projectDir" -ForegroundColor Cyan

    # 遍历子目录
    Get-ChildItem -Path $projectDir -Directory | ForEach-Object {
        $subjectDir = $_.FullName
        Write-Host "发现子目录: $subjectDir"

        # 查找.xos文件（仅当前目录）
        $xosFiles = Get-ChildItem -Path $subjectDir -Filter "*.xos" -File
        
        if ($xosFiles.Count -gt 0) {
            # 移动文件
            $xosFiles | ForEach-Object {
                Write-Host "正在移动文件: $($_.Name)"
                Move-Item -Path $_.FullName -Destination $projectDir -Force
            }
            
            # 删除空目录
            try {
                Remove-Item -Path $subjectDir -Recurse -Force
                Write-Host "已删除目录: $subjectDir" -ForegroundColor Green
            }
            catch {
                Write-Host "删除目录失败: $_" -ForegroundColor Red
            }
        }
        else {
            Write-Host "未找到.xos文件，跳过目录: $subjectDir" -ForegroundColor Yellow
        }
    }
}

Write-Host "所有操作已完成！" -ForegroundColor Green