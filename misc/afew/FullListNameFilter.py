from afew.FilterRegistry import register_filter
from afew.filters.HeaderMatchingFilter import HeaderMatchingFilter

@register_filter
class FullListNameFilter(HeaderMatchingFilter):
    message = 'Tagging mailing list posts'
    query = 'NOT tag:lists'
    pattern = r"<(?P<list_id>[a-z][a-z0-9.-]*)"
    header = 'List-Id'
    tags = ['+lists', '+lists/{list_id}']
