from pyswip import Prolog
from openai import OpenAI
import os

# Call GPT-4-turbo (or 4o if preferred)
client = OpenAI(
    # This is the default and can be omitted
    api_key=os.environ.get("OPENAI_API_KEY"),
)

response = client.responses.create(
    model="o3-2025-04-16",
    instructions="You are a Prolog assistant. Translate high-level restaurant optimization goals into working Prolog rules.",
    input="Model a restaurant that has 3 machines: oven, fryer, and grill. Dishes have prep times and profits. Max 2 machines can be used at once. Find plans that maximize profit given limited availability.",
)

generated_rules = response.output_text

# Save rules to file
with open("kitchen_rules.pl", "w") as f:
    f.write(generated_rules)

# Run Prolog query
prolog = Prolog()
prolog.consult("kitchen_rules.pl")

plans = list(prolog.query("plan(Dishes, Profit)"))

# Display result
print("\nTop Kitchen Plans:\n")
for p in sorted(plans, key=lambda x: -x['Profit']):
    print(f"Plan: {p['Dishes']}, Profit: ${p['Profit']}")
