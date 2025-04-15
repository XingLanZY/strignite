# strignite

一个高性能、类型安全且功能丰富的C++字符串库，支持多种字符编码和现代C++特性，助力高效字符串处理！

## 主要特性 ✨

- **多字符类型支持**  
  `char`/`wchar_t`/`char16_t`/`char32_t`/`char8_t`（C++20）全支持

- **丰富字符串类型**  
  - `basic_string_view`: 轻量只读视图
  - `basic_string`: 不可变字符串（SSO优化、引用计数）
  - `basic_mutable_string`: 可变动态字符串
  - `basic_static_string`: 编译期固定容量字符串

- **高效算法** 🔍  
  - Boyer-Moore/Horspool快速搜索
  - Two-way复杂模式匹配
  - 支持正向/反向搜索

- **编码转换** 🔄  
  UTF-8/16/32互转，自动处理大小端

- **现代C++特性** 🆕  
  - constexpr支持编译期计算
  - C++11到C++20特性适配
  - 类型安全的API设计

- **实用功能** 🧰  
  - 字符串分割/拼接/反转
  - 大小写转换（ASCII/Latin-1）
  - 数字解析/类型转换
  - 空白修剪/填充对齐

## 快速开始 🚦

### 包含头文件
#include "string.hpp"

基础使用示例
using namespace bcs;

// 创建字符串
string s1 = "Hello"; 
static_string<32> s2 = u8"😊UTF-8";
mutable_string s3 = L"动态字符串";

// 字符串操作
auto pos = s1.find("ll"); // Boyer-Moore搜索
auto upper = s1.to_upper<ascii>(); 

// 编码转换
u16string utf16 = s1.to_utf16();
u32string utf32 = s2.to_utf32();

// 字符串生成
auto joined = join({"A","B","C"}, "|"); // A|B|C
auto repeated = s1 * 3; // HelloHelloHello

API 亮点 💡

字符串视图
string_view sv = "View";
sv.starts_with("V"); // true
sv.substring(1, 2); // "ie"

可变字符串
mutable_string s;
s.reserve(100);
s += "Append";
s.prepend("Pre+");
s.shrink_to_fit();

高效搜索
auto pos = boyer_moore_search(
    haystack, haystack_len,
    needle, needle_len
);

性能优化 🚀
SSO优化：短字符串零堆分配

引用计数：大字符串写时复制

算法优化：Boyer-Moore预处理加速搜索

内存对齐：针对不同架构优化

兼容性要求 📋
编译器：支持C++11及以上（推荐C++20）

标准库：依赖STL组件（<string>/<vector>等）

平台：Windows/Linux/macOS，自动检测字节序

贡献指南 🤝
欢迎通过Issue和PR参与贡献！
请确保：

代码符合项目风格

添加对应单元测试

更新相关文档

许可证 📜
本项目采用 MIT License，详情见项目根目录许可证文件。