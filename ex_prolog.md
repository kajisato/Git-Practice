Prolog problems -1. List
-------------------------
problems from https://sites.google.com/site/prologsite/prolog-problems/1


###1.01 Find the last element of a list.
List は a (= [a0,a1,..., an]) として
####C++
#####array
```c++
//int a[n+1];
a[sizeof(a)-1]
*(a+sizeof(a)-1)
```
#####list
```c++
//#include<list>
//std::list<int> a;
*(--a.end())
```

#### Python
```python
a[-1]
```
#### Ruby
```ruby
a[-1]
```
or
```ruby
a.last
```

#### Haskell
```haskell
last a
```

#### Lua
```lua
a[#a]
```
#### and perhaps a lot more..



(見出しレベルを一つづつ下げてみた)
