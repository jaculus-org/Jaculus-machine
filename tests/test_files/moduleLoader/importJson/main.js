import data from './data.json' with { type: 'json' };
report(data.name);
report(data.value);
report(data.nested.flag);
exit(0);
