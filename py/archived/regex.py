import re






result = re.findall(r'\d', 'a3bc4')
print(result)

print(re.findall(r'[\w\d]{18}', "SKJ52AA282C08E5E93 SKJ50618268F230B08 SKJ50618268F230B08 SKJ53D86B316A2F085 SKJ5003FE4C799FCFC"))

