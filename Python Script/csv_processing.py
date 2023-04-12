import csv
import numpy as np
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from io import BytesIO
import io
import os
import base64
from reportlab.lib.pagesizes import letter
from reportlab.lib import colors
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Paragraph, Spacer, Image
from reportlab.lib.styles import getSampleStyleSheet

# Define a function to return the height class code based on the height value
def get_height_class_code(height):
    if 0 <= height < 10.5:
        return 1
    elif 10.5 <= height < 19.5:
        return 2
    elif 19.5 <= height < 28.5:
        return 3
    elif 28.5 <= height < 37.5:
        return 4
    elif 37.5 <= height < 46.5:
        return 5
    elif 46.5 <= height < 55.5:
        return 6
    elif 55.5 <= height < 64.5:
        return 7
    elif height >= 64.5:
        return 8
    else:
        return None

# calculating tree health based on ExG index
def get_tree_health(row):
    r, g, b = float(row[3]), float(row[4]), float(row[5])
    exg = 2 * g - r - b
    threshold = 0.2 # Decided to put 0.2 based on https://www.researchgate.net/figure/Excess-green-ExG-histogram-for-vegetation-classification-with-the-Otsu-threshold-value_fig6_324234218
    return 1 if exg > threshold else 0

def generate_summary_paragraph(count, mean_height, mean_area):
    height_inference = ""
    area_inference = ""
    
    if mean_height > 20:
        height_inference = "The trees in the forest are generally tall, which may indicate a mature forest."
    elif mean_height > 10:
        height_inference = "The forest has a mix of young and mature trees, suggesting a balanced ecosystem."
    else:
        height_inference = "The trees in the forest are generally shorter, indicating a younger forest or a forest with smaller tree species."
    
    if mean_area > 50:
        area_inference = "The tree coverage areas are relatively large, implying a lower tree density in the forest."
    elif mean_area > 25:
        area_inference = "The tree coverage areas vary, which could indicate a diverse mix of tree densities and species in the forest."
    else:
        area_inference = "The tree coverage areas are relatively small, suggesting a densely populated forest."
    
    summary_text = f"""
    The dataset contains information on {count:.0f} tree segments. On average, the trees have a height of {mean_height:.2f} units, 
    and the tree coverage areas, represented by convex hull areas, have an average size of {mean_area:.2f} square units.

    {height_inference} {area_inference}

    These statistics provide an overview of the tree heights and coverage areas in the analyzed forest. The information can be helpful 
    for understanding the forest's structure, health, and biodiversity and for making informed decisions about forest management and conservation efforts.
    """

    return Paragraph(summary_text, styles['BodyText'])

# Open the input and output files
# crown_metrics.csv is the input file name, can be changed accordingly to what the input file name is
# output.csv is the file that will be created consiting of the clean data
with open('sample.csv', 'r') as input_file, open('sample_output.csv', 'w', newline='') as output_file:
    # Preprocess the input file content to make it readable as a CSV
    input_contents = input_file.read().replace('\n', '').replace("list(c(",'').replace("))",'').replace('""', '"\n"')
    input_file = io.StringIO(input_contents)

    csv_reader = csv.reader(input_file)
    csv_writer = csv.writer(output_file)

    # Read and modify the header row
    header = next(csv_reader)
    header.pop(-4)
    header.pop(-5)
    header.append('Height Class Code')
    header.append('Tree Health')
    csv_writer.writerow(header[1:])

    # Process each row in the input file
    for row in csv_reader:
        # Remove unnecessary columns
        row.pop(1)
        row.pop(2)

        # Initialize the output row
        modified_row = []

        # Extract the last three columns as strings
        str1, str2, str3 = row[-3:]

        # Convert the string values to lists of integers
        int_list1 = [int(s) for s in str1.split(',')]
        int_list2 = [int(s) for s in str2.split(',')]
        int_list3 = [int(s) for s in str3.split(',')]

        # Calculate the average of each integer list
        avg1 = round((sum(int_list1) / len(int_list1))*100)/100
        avg2 = round((sum(int_list2) / len(int_list2))*100)/100
        avg3 = round((sum(int_list3) / len(int_list3))*100)/100

        # Replace the original string values with the average values in the output row
        row[-3:] = [str(avg1), str(avg2), str(avg3)]

        # Process each element in the row
        for element in row:
            try:
                # Attempt to convert the element to a float
                element_float = float(element)

                # If the float is less than or equal to 10000, add it to the output row
                if element_float <= 10000:
                    modified_row.append(element)
            except ValueError:
                # If the element cannot be converted to a float, add it to the output row as is
                modified_row.append(element)
                
        height = float(row[header.index('Z')])
        modified_row.append(get_height_class_code(height))
        modified_row.append(get_tree_health(modified_row))

        # Write the modified row to the output file
        csv_writer.writerow(modified_row)

# Read the CSV file and compute the required values
height_class_codes = []
heights = []
areas = []
tree_colors = []
tree_segments_count = 0
forest_coverage_area = 0
filename = 'sample_output.csv'
file_data = pd.read_csv(filename)

with open(filename, mode="r") as csvfile:
    csv_reader = csv.reader(csvfile)
    header = next(csv_reader)

    for row in csv_reader:
        height = float(row[header.index('Z')])
        area = float(row[header.index('convhull_area')])
        r = round(float(row[header.index('R')]))
        g = round(float(row[header.index('G')]))
        b = round(float(row[header.index('B')]))

        tree_colors.append((r, g, b))
        heights.append(height)
        areas.append(area)
        height_class_codes.append(int(row[header.index('Height Class Code')]))
        tree_segments_count += 1
        forest_coverage_area += float(row[header.index('convhull_area')])

forest_coverage_area = forest_coverage_area

# Generate the histogram data
hist_data, bin_edges, _ = plt.hist(height_class_codes, bins=range(1, 10), align='left', rwidth=0.8)

# Add count labels on top of the bars
for i in range(len(hist_data)):
    count = hist_data[i]
    x_position = bin_edges[i] # Centering the label on the bar
    y_position = count + 0.2  # Slightly above the bar
    plt.text(x_position, y_position, str(int(count)), ha='center', va='bottom')

plt.xlabel("Height Class Code")
plt.ylabel("Count")
plt.title("Distribution of Height Class Codes")
plt.xticks(range(1, 9))

buf = BytesIO()
plt.savefig(buf, format='png', dpi=300)
plt.clf()
buf.seek(0)
height_class_codes_image = base64.b64encode(buf.read()).decode("utf-8")
buf.close()

# Generate Height Distribution Plot
plt.hist(heights, bins=20)
plt.xlabel('Height (Z)')
plt.ylabel('Frequency')
plt.title('Height Distribution')

buf_height = BytesIO()
plt.savefig(buf_height, format='png', dpi=300)
plt.clf()
buf_height.seek(0)
height_distribution_image = base64.b64encode(buf_height.read()).decode("utf-8")
buf_height.close()

# Generate Area Distribution Plot
plt.hist(areas, bins=20)
plt.xlabel('Convex Hull Area')
plt.ylabel('Frequency')
plt.title('Area Distribution')

buf_area = BytesIO()
plt.savefig(buf_area, format='png', dpi=300)
plt.clf()
buf_area.seek(0)
area_distribution_image = base64.b64encode(buf_area.read()).decode("utf-8")
buf_area.close()

# Color distribution
r_values, g_values, b_values = zip(*tree_colors)

plt.hist(r_values, bins=20, color='red', alpha=0.5, label='R')
plt.hist(g_values, bins=20, color='green', alpha=0.5, label='G')
plt.hist(b_values, bins=20, color='blue', alpha=0.5, label='B')
plt.xlabel('Color Value')
plt.ylabel('Frequency')
plt.title('Color Distribution')
plt.legend()

buf_color_hist = BytesIO()
plt.savefig(buf_color_hist, format='png', dpi=300)
plt.clf()
buf_color_hist.seek(0)
color_hist_image = base64.b64encode(buf_color_hist.read()).decode("utf-8")
buf_color_hist.close()

plt.scatter(heights, areas, alpha=0.5)
plt.xlabel("Height (Z)")
plt.ylabel("Convex Hull Area")
plt.title("Height vs. Convex Hull Area")

buf_height_area = BytesIO()
plt.savefig(buf_height_area, format='png', dpi=300)
plt.clf()
buf_height_area.seek(0)
height_area_image = base64.b64encode(buf_height_area.read()).decode("utf-8")
buf_height_area.close()


# Create the violin plot
sns.violinplot(data=file_data[['Z','convhull_area']])
plt.title("Violin Plot of Tree Heights and Convex Hull Areas")
plt.ylabel("Value")
plt.xticks(ticks=[0, 1], labels=['Height', 'Convhull Area'])

buf_violin = BytesIO()
plt.savefig(buf_violin, format='png', dpi=300)
plt.clf()
buf_violin.seek(0)
violin_image = base64.b64encode(buf_violin.read()).decode("utf-8")
buf_violin.close()

healthy_tree_count = len(file_data[file_data['Tree Health'] == 1])
dead_tree_count = len(file_data[file_data['Tree Health'] == 0])

count = len(file_data)
healthy_tree_percentage = healthy_tree_count / count * 100
dead_tree_percentage = dead_tree_count / count * 100

tree_health_labels = ['Healthy', 'Dead']
tree_health_values = [healthy_tree_percentage, dead_tree_percentage]

fig, ax = plt.subplots()
ax.pie(tree_health_values, labels=tree_health_labels, autopct='%1.1f%%', startangle=90)
ax.axis('equal')  # Equal aspect ratio ensures the pie chart is circular.

# Draw a white circle in the middle to create a donut chart
center_circle = plt.Circle((0, 0), 0.70, fc='white')
fig.gca().add_artist(center_circle)

plt.title('Tree Health Distribution')
buf = BytesIO()
plt.savefig(buf, format='png', dpi=300)
plt.clf()
buf.seek(0)
tree_health_image = base64.b64encode(buf.read()).decode("utf-8")
buf.close()


# Generate the summary statistics using the describe() function
summary_stats = file_data.describe()

# Extract key statistics
count = summary_stats.loc['count', 'Z']
mean_height = summary_stats.loc['mean', 'Z']
mean_area = summary_stats.loc['mean', 'convhull_area']
min_height = summary_stats.loc['min', 'Z']
max_height = summary_stats.loc['max', 'Z']
min_area = summary_stats.loc['min', 'convhull_area']
max_area = summary_stats.loc['max', 'convhull_area']
std_height = summary_stats.loc['std', 'Z']
std_area = summary_stats.loc['std', 'convhull_area']

# Create the PDF report
doc = SimpleDocTemplate("tree_analysis_report.pdf", pagesize=letter, rightMargin=30, leftMargin=30, topMargin=30, bottomMargin=30)
styles = getSampleStyleSheet()

# Add the introduction page
title = Paragraph("Aurora UAV | Analyzing Trees using Drone Imagery", styles['Heading1'])
intro_text = """
This report presents an analysis of tree height classes and forest coverage based on drone imagery data. The data includes tree segment information collected from LIDAR point cloud. The analysis includes the distribution of height class codes, the total count of tree segments, and the total forest coverage area.
"""
intro = Paragraph(intro_text, styles['BodyText'])

# Add a table with the total tree count and total forest coverage area
data = [["Total Tree Segments", tree_segments_count], ["Total Forest Coverage Area (sq.m)", forest_coverage_area]]
table = Table(data)
table.setStyle(TableStyle([('BACKGROUND', (0, 0), (-1, 0), colors.grey), ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
                           ('ALIGN', (0, 0), (-1, -1), 'CENTER'), ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
                           ('FONTSIZE', (0, 0), (-1, 0), 14), ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
                           ('BACKGROUND', (0, 1), (-1, -1), colors.beige), ('GRID', (0, 0), (-1, -1), 1, colors.black)]))

criteria_data = [["Height Class Code", "Height Range (m)"],
                 [1, "0 - 5"],
                 [2, "5 - 10"],
                 [3, "10 - 15"],
                 [4, "15 - 20"],
                 [5, "20 - 25"],
                 [6, "25 - 30"],
                 [7, "30 - 35"],
                 [8, "35 - 40"]]
criteria_table = Table(criteria_data)
criteria_table.setStyle(TableStyle([('BACKGROUND', (0, 0), (-1, 0), colors.grey),
                                    ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
                                    ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
                                    ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
                                    ('FONTSIZE', (0, 0), (-1, 0), 14),
                                    ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
                                    ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
                                    ('GRID', (0, 0), (-1, -1), 1, colors.black)]))


# Add visualizations as images
image_data = base64.b64decode(height_class_codes_image)
image = Image(BytesIO(image_data), width=450,height=300)

image_height_data = base64.b64decode(height_distribution_image)
image_height = Image(BytesIO(image_height_data), width=450, height=300)

image_area_data = base64.b64decode(area_distribution_image)
image_area = Image(BytesIO(image_area_data), width=450, height=300)

image_color_hist_data = base64.b64decode(color_hist_image)
image_color_hist = Image(BytesIO(image_color_hist_data), width=450, height=400)

image_height_area_data = base64.b64decode(height_area_image)
image_height_area = Image(BytesIO(image_height_area_data), width=450, height=300)

image_violin_data = base64.b64decode(violin_image)
image_violin = Image(BytesIO(image_violin_data), width=450, height=325)

image_data = base64.b64decode(tree_health_image)
tree_health_plot = Image(BytesIO(image_data), width=550, height=450)

# Add meaningful descriptions for the table and graphs
table_description = Paragraph(
    "The table below shows the total count of tree segments and the total forest coverage area in square meters:",
    styles['BodyText'])

graph_description = Paragraph(
    "The bar graph represents the distribution of height class codes among the tree segments:",
    styles['BodyText'])

height_distribution_heading = Paragraph("Height Distribution", styles['Heading2'])
height_distribution_description = Paragraph(
    "The histogram below shows the distribution of tree heights in the analyzed data:",
    styles['BodyText'])

area_distribution_heading = Paragraph("Area Distribution", styles['Heading2'])
area_distribution_description = Paragraph(
    "The histogram below shows the distribution of convex hull areas in the analyzed data:",
    styles['BodyText'])

criteria_table_description = Paragraph(
    "The height class codes are assigned based on the following height ranges:",
    styles['BodyText'])

color_hist_heading = Paragraph("Color Distribution", styles['Heading2'])
color_hist_description = Paragraph(
    "The histogram below displays the color distribution of trees in the dataset based on the RGB color channels. "
    "Analyzing the color distribution can provide insights into the overall health and biodiversity of the forest. "
    "For instance, a higher proportion of green values might indicate a healthy, thriving forest, while higher red or "
    "blue values may signify stressed or dying trees. Furthermore, variations in color could highlight differences "
    "in tree species or seasonal changes.",
    styles['BodyText'])

height_area_heading = Paragraph("Height vs. Convex Hull Area", styles['Heading2'])
height_area_description = Paragraph(
    "The scatter plot below shows the relationship between tree height and convex hull area. "
    "This visualization can help identify any correlation between tree height and the area they occupy, "
    "which might be useful for understanding tree growth patterns and forest density.",
    styles['BodyText'])

violin_heading = Paragraph("Violin Plot of Tree Heights and Convex Hull Areas", styles['Heading2'])
violin_description = Paragraph(
    "The violin plot below shows how tree heights and convex hull areas are spread out. "
    "This picture helps us understand how the data is arranged and how common different values are. "
    "The wider the violin shape at any point, the more data there is for that value. The white dot in the middle tells us the middle value of the data.",
    styles['BodyText'])

summary_heading = Paragraph("Summary and Insights", styles['Heading2'])
summary = generate_summary_paragraph(count, mean_height, mean_area)

# Heading for the tree health section
tree_health_heading = Paragraph("Tree Health Distribution", styles['Heading2'])
tree_health_description_text = """
The donut chart below presents the distribution of tree health in the analyzed area. The chart categorizes trees into two groups: healthy and dead. Understanding the proportion of healthy trees to dead trees is crucial for forest management and conservation efforts. This information can help identify areas that may require targeted intervention, such as tree planting or pest control measures, to maintain overall forest health and biodiversity.
"""
tree_health_description = Paragraph(tree_health_description_text, styles['BodyText'])


doc.build([title, Spacer(1, 12), intro, Spacer(1, 12),
           table_description, table, Spacer(1, 12),
           criteria_table_description, criteria_table, Spacer(1, 12),
           graph_description, image, Spacer(1, 12),
           height_distribution_heading, height_distribution_description, Spacer(1, 12),
           image_height, Spacer(1, 12),
           area_distribution_heading, area_distribution_description, Spacer(1, 12),
           image_area, Spacer(1, 12),
           color_hist_heading, color_hist_description, Spacer(1, 3),
           image_color_hist, Spacer(1, 3),
           height_area_heading, height_area_description, Spacer(1, 3),
           image_height_area, Spacer(1, 3),
           violin_heading, violin_description, Spacer(1, 3),
           image_violin, Spacer(1,3),
           tree_health_heading, tree_health_description, Spacer(1, 12),
           tree_health_plot, Spacer(1, 12),
           summary_heading, summary])

# Close the plots
plt.close()