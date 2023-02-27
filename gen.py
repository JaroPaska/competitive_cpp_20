import sys

def get_include(line):
    if line.startswith('#include') and line.count('"') == 2:
        return line[line.index('"') + 1 : line.rindex('"')]
    return None

def resolve_includes(file_path, included=set()):
    output_file = ''
    with open(file_path, 'r') as file:
        print(f'Resolving includes {file_path}')
        while True:
            line = file.readline()
            if not line:
                break

            include = get_include(line)
            if include:
                if include not in included:
                    output_file += resolve_includes(f'{include}', included=included) + '\n\n'
                    included.add(include)
            elif not line.startswith('#pragma once'):
                output_file += line
    return output_file

parts = sys.argv[1].split('.')
submit_name = '.'.join(parts[0:-1])
submit_ext = parts[-1]
submit_path = f'{submit_name}.submit.{submit_ext}'
submit = resolve_includes(sys.argv[1])
with open(submit_path, 'w') as file:
    file.write(submit)
    print(f'Wrote {submit_path}')

